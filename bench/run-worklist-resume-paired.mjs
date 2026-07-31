import { spawnSync } from "node:child_process";
import crypto from "node:crypto";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";

const [
  executable,
  baselinePattern,
  candidatePattern,
  outputCsv,
  warmupText = "2",
  pairsText = "21",
  stdevText = "5",
] = process.argv.slice(2);

if (!executable || !baselinePattern || !candidatePattern || !outputCsv) {
  console.error(
    "usage: node bench/run-worklist-resume-paired.mjs EXE BASELINE_PATTERN CANDIDATE_PATTERN OUTPUT.csv [WARMUP_PAIRS] [PAIRS] [STDEV_PERCENT]",
  );
  process.exit(2);
}

const warmupPairs = positiveInteger(warmupText, "warm-up pairs");
const finalPairs = positiveInteger(pairsText, "final pairs");
const stdevPercent = positiveNumber(stdevText, "standard-deviation target");
const bootstrapSeed = 0x50b0_2026;
const bootstrapSamples = 100_000;
const temporaryDirectory = fs.mkdtempSync(
  path.join(os.tmpdir(), "pure-borrow-worklist-resume-"),
);

function positiveInteger(text, label) {
  const value = Number(text);
  if (!Number.isInteger(value) || value <= 0) {
    throw new Error(`invalid ${label}: ${text}`);
  }
  return value;
}

function positiveNumber(text, label) {
  const value = Number(text);
  if (!Number.isFinite(value) || value <= 0) {
    throw new Error(`invalid ${label}: ${text}`);
  }
  return value;
}

function sha256(file) {
  return crypto.createHash("sha256").update(fs.readFileSync(file)).digest("hex");
}

function measure(pattern, label, pair) {
  const csv = path.join(temporaryDirectory, `${label}-${pair}.csv`);
  const result = spawnSync(
    executable,
    [
      "-p",
      pattern,
      "--stdev",
      String(stdevPercent),
      "--csv",
      csv,
      "-j1",
      "--time-mode",
      "wall",
      "--hide-progress",
      "+RTS",
      "-N1",
      "-RTS",
    ],
    {
      encoding: "utf8",
      timeout: 120_000,
    },
  );
  if (result.status !== 0) {
    throw new Error(
      `${label} pair ${pair} failed:\n${result.stdout}\n${result.stderr}`,
    );
  }
  const lines = fs.readFileSync(csv, "utf8").trim().split("\n");
  const fields = lines.at(-1).split(",");
  const meanPicoseconds = Number(fields[1]);
  if (!Number.isFinite(meanPicoseconds) || meanPicoseconds <= 0) {
    throw new Error(`invalid benchmark mean in ${csv}`);
  }
  return meanPicoseconds;
}

function runPair(pair, warmup) {
  const baselineFirst = pair % 2 === 1;
  let baselinePicoseconds;
  let candidatePicoseconds;
  if (baselineFirst) {
    baselinePicoseconds = measure(
      baselinePattern,
      warmup ? "warmup-baseline" : "baseline",
      pair,
    );
    candidatePicoseconds = measure(
      candidatePattern,
      warmup ? "warmup-candidate" : "candidate",
      pair,
    );
  } else {
    candidatePicoseconds = measure(
      candidatePattern,
      warmup ? "warmup-candidate" : "candidate",
      pair,
    );
    baselinePicoseconds = measure(
      baselinePattern,
      warmup ? "warmup-baseline" : "baseline",
      pair,
    );
  }
  const ratio = candidatePicoseconds / baselinePicoseconds;
  process.stdout.write(
    `${warmup ? "warmup" : "pair"} ${pair}/${warmup ? warmupPairs : finalPairs}` +
      ` ${baselineFirst ? "baseline-first" : "candidate-first"}` +
      ` baseline=${(baselinePicoseconds / 1e6).toFixed(3)}us` +
      ` candidate=${(candidatePicoseconds / 1e6).toFixed(3)}us` +
      ` ratio=${ratio.toFixed(5)}\n`,
  );
  return {
    pair,
    baselineFirst,
    baselinePicoseconds,
    candidatePicoseconds,
    ratio,
  };
}

for (let pair = 1; pair <= warmupPairs; pair += 1) {
  runPair(pair, true);
}

const rows = [];
for (let pair = 1; pair <= finalPairs; pair += 1) {
  rows.push(runPair(pair, false));
}

const logRatios = rows.map(({ ratio }) => Math.log(ratio));
const geometricMean = Math.exp(
  logRatios.reduce((total, value) => total + value, 0) / logRatios.length,
);

let randomState = bootstrapSeed;
function randomIndex(limit) {
  randomState ^= randomState << 13;
  randomState ^= randomState >>> 17;
  randomState ^= randomState << 5;
  return (randomState >>> 0) % limit;
}

const bootstrapRatios = [];
for (let sample = 0; sample < bootstrapSamples; sample += 1) {
  let total = 0;
  for (let index = 0; index < logRatios.length; index += 1) {
    total += logRatios[randomIndex(logRatios.length)];
  }
  bootstrapRatios.push(Math.exp(total / logRatios.length));
}
bootstrapRatios.sort((left, right) => left - right);
const upper95 =
  bootstrapRatios[Math.ceil(0.95 * bootstrapRatios.length) - 1];

fs.writeFileSync(
  outputCsv,
  [
    "pair,order,baseline_ps,candidate_ps,ratio",
    ...rows.map(
      ({
        pair,
        baselineFirst,
        baselinePicoseconds,
        candidatePicoseconds,
        ratio,
      }) =>
        `${pair},${baselineFirst ? "baseline-first" : "candidate-first"},` +
        `${baselinePicoseconds},${candidatePicoseconds},${ratio}`,
    ),
    "",
  ].join("\n"),
);

const reportPath = `${outputCsv}.json`;
const runnerPath = fileURLToPath(import.meta.url);
fs.writeFileSync(
  reportPath,
  `${JSON.stringify(
    {
      executable,
      executableSha256: sha256(executable),
      runnerSha256: sha256(runnerPath),
      baselinePattern,
      candidatePattern,
      protocol: {
        warmupPairs,
        finalPairs,
        stdevPercent,
        timeMode: "wall",
        capabilities: 1,
        order:
          "odd pairs baseline/candidate; even pairs candidate/baseline; fresh process per observation",
        outlierPolicy: "none",
      },
      analysis: {
        geometricMean,
        oneSided95UpperConfidenceBound: upper95,
        bootstrapSeed: `0x${bootstrapSeed.toString(16)}`,
        bootstrapSamples,
      },
      rows,
    },
    null,
    2,
  )}\n`,
);

fs.rmSync(temporaryDirectory, { recursive: true });
process.stdout.write(
  `final geometric_mean=${geometricMean.toFixed(6)}` +
    ` one_sided_95_ucb=${upper95.toFixed(6)}` +
    ` seed=0x${bootstrapSeed.toString(16)}` +
    ` samples=${bootstrapSamples} outliers=none\n`,
);
