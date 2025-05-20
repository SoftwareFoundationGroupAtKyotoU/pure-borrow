module.exports = async ({ github, context, core, glob, io, require }) => {
  const fs = require("fs");
  const globber = await glob.create("ci/configs/*.project");
  const files = await globber.glob();

  function generate_obj(fullpath) {
    const is_head = /-head\.project$/.test(fullpath);
    const path = fullpath.replace(/^.+\//, "ci/configs/");
    const name = fullpath.replaceAll(/.+\/|\.project$/g, "");
    const match = fs
      .readFileSync(fullpath, "utf-8")
      .match(/with-compiler:\s*ghc-([\d\.]+)/);
    const ghc = match[1];

    return { path, name, ghc, is_head };
  }

  const plans = files.map(generate_obj);
  core.info(`plan: ${JSON.stringify(plans)}`);
  core.setOutput("plan", JSON.stringify(plans));
};
