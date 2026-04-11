# Gnuplot script for qsort benchmark plots
# Usage: gnuplot -e "input='path/to/data.csv'" scripts/genplot.gnuplot
# If input is not set, defaults to the CSV name derived from git rev.

if (!exists("input")) input = "qsort.csv"
if (!exists("output")) output = "qsort.png"

set terminal pngcairo enhanced color size 1600,700 font "Latin Modern Roman,12"
set output output

set datafile separator ","

# ── Colours & styles ─────────────────────────────────────────────────
# Intro          : black  solid line, no markers
# Sequential     : red    solid line, no markers
# Naïve  4 / 16 / 32 : blue / green / black, open triangles
# WS     4 /  8 / 10 : blue / green / black, + markers

set style line 1 lc rgb "black" lw 1.4 dt 1          # Intro
set style line 2 lc rgb "red"   lw 1.0 dt 1          # Sequential
set style line 3 lc rgb "blue"  lw 1.0 dt 1 pt 8 ps 1.8   # Naive 4  (open triangle up)
set style line 4 lc rgb "green" lw 1.0 dt 1 pt 8 ps 1.8   # Naive 16 (open triangle up)
set style line 5 lc rgb "black" lw 1.0 dt 1 pt 8 ps 1.8   # Naive 32 (open triangle up)
set style line 6 lc rgb "blue"  lw 1.0 dt 1 pt 1 ps 1.8   # WS 4     (+)
set style line 7 lc rgb "green" lw 1.0 dt 1 pt 1 ps 1.8   # WS 8     (+)
set style line 8 lc rgb "black" lw 1.0 dt 1 pt 1 ps 1.8   # WS 10    (+)

# ── Layout (manual positioning) ──────────────────────────────────────
# Reserve bottom 20% for the shared legend
legend_h = 0.20
plot_b   = legend_h
plot_t   = 0.98
plot_gap = 0.08

set multiplot
unset key

# ── Left panel: Wall Clock Time ──────────────────────────────────────
set lmargin at screen 0.07
set rmargin at screen 0.48
set bmargin at screen plot_b
set tmargin at screen plot_t
set xlabel 'N'
set ylabel 'Wall Clock Time [ms]'

plot \
  input using "size":"introMean"       with lines      ls 1 notitle, \
  input using "size":"sequentialMean"   with lines      ls 2 notitle, \
  input using "size":"parallel4Mean"    with linespoints ls 3 notitle, \
  input using "size":"parallel16Mean"   with linespoints ls 4 notitle, \
  input using "size":"parallel32Mean"   with linespoints ls 5 notitle, \
  input using "size":"workSteal4Mean"   with linespoints ls 6 notitle, \
  input using "size":"workSteal8Mean"   with linespoints ls 7 notitle, \
  input using "size":"workSteal10Mean"  with linespoints ls 8 notitle

# ── Right panel: Allocation ──────────────────────────────────────────
set lmargin at screen 0.56
set rmargin at screen 0.97
set bmargin at screen plot_b
set tmargin at screen plot_t
set xlabel 'N'
set ylabel 'Allocation [MB]'

plot \
  input using "size":"introAlloc"       with lines      ls 1 notitle, \
  input using "size":"sequentialAlloc"   with lines      ls 2 notitle, \
  input using "size":"parallel4Alloc"    with linespoints ls 3 notitle, \
  input using "size":"parallel16Alloc"   with linespoints ls 4 notitle, \
  input using "size":"parallel32Alloc"   with linespoints ls 5 notitle, \
  input using "size":"workSteal4Alloc"   with linespoints ls 6 notitle, \
  input using "size":"workSteal8Alloc"   with linespoints ls 7 notitle, \
  input using "size":"workSteal10Alloc"  with linespoints ls 8 notitle

# ── Shared legend (dummy plot spanning full width) ───────────────────
set lmargin at screen 0.07
set rmargin at screen 0.97
set bmargin at screen 0.0
set tmargin at screen legend_h
unset xlabel
unset ylabel
unset tics
unset border
set xrange [0:1]
set yrange [0:1]
set key center center horizontal samplen 2 spacing 1.2 \
    font ",15" maxrows 1

plot \
  NaN with lines      ls 1 title 'Intro', \
  NaN with lines      ls 2 title 'Sequential', \
  NaN with linespoints ls 3 title "Naive 4", \
  NaN with linespoints ls 4 title "Naive 16", \
  NaN with linespoints ls 5 title "Naive 32", \
  NaN with linespoints ls 6 title 'WS 4', \
  NaN with linespoints ls 7 title 'WS 8', \
  NaN with linespoints ls 8 title 'WS 10'

unset multiplot
