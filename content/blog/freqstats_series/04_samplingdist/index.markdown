---
weight:       5
title:        The Sampling Distribution
subtitle:     The Nuts and Bolts of frequentist statistics.  
date:         2022-10-01
draft:        false
excerpt:      |
  Part 4 of a series on the fundamentals of frequentist statistics.
---



<style type="text/css">

/*---GENERAL TEXT---*/

/* text */
p {
  font-size: 14px;
  text-align: justify;
  width: auto;
}

/* text classes */
p.keypoint{
  border-left: 0.7em solid #c4c4c4;
  background-color: #ebebeb;
  padding: 0.75em 1em 0.75em 0.6em;
}

p.figure{
  text-align: center;
}

p.caption{
  text-align: justify; 
  font-size: 12px;
  margin: -1em 6em 2em 6em;
}

p.table{
  font-size: 10px;
}

/* FIGURE X */
b.figreftext{
  font-size: 12px;
  font-family: "Times new Roman", "Times", "sans-serif";
  color: #8a0900;
  margin-left: .2em;
}
b.figreffig{
  font-size: 11px;
  font-family: "Times new Roman", "Times", "sans-serif";
  color: #8a0900;
}
b.key{
  color: #8a0900;
}

/* list */
li{
  font-size: 14px;
  text-align: justify;
  padding-left: 1em;
}

/* headings */
h2.small{
  font-size: 20px;
  margin-top: 1.5em;
}

/*---FOOTER FOOTNOTES---*/

/* footer, which will hold footnotes as ordered list */
footer {
  margin-top: 5ex;
  padding-top: 2ex;
  margin-bottom: -5ex;
  border-top: 1px solid silver;
  font-size: 1em;
}
/* footer ordered list */
footer ol {
  padding-left: 1em;
}

/* init a `footnotes` counter*/
article {
  counter-reset: footnotes;
}

/*  inline footnotes references
 *  1. increment the counter at each new reference
 *  2. reset link styles to make it appear like regular text
 */
[aria-describedby="footnote-label"] {
  counter-increment: footnotes; /* 1 */
  text-decoration: none; /* 2 */
  color: inherit; /* 2 */
  cursor: default; /* 2 */
  outline: none; /* 2 */
}

/*  actual numbered references
 *  1. display the current state of the counter (e.g. `[1]`)
 *  2. align text as superscript
 *  3. make the number smaller (since it's superscript)
 *  4. slightly offset the number from the text
 *  5. reset link styles on the number to show it's usable
 */
[aria-describedby="footnote-label"]::after {
  content: '[' counter(footnotes) ']';    /* 1 */
  vertical-align: super;                  /* 2 */
  font-size: 0.8em;                       /* 3 */
  margin-left: 2px;                       /* 4 */
  color: #8a0900;                         /* 5 */
  cursor: pointer;                        /* 5 */
}

/* resetting the default focused styles on the number
 */
[aria-describedby="footnote-label"]:focus::after {
  outline: thin dotted;
  outline-offset: 2px;
}

[aria-label="Back to content"] {
  font-size: 0.8em;
}

/* highlight target note
 */
footer :target {
  background: #FEF6BB;
}

/* visually hidden yet accessible content
 */
.visually-hidden {
  position: absolute;
  clip: rect(0 0 0 0);
  visibility: hidden;
  opacity: 0;
}

/* TABLE */

table.list {
  border-collapse: collapse;
  border: none;
  margin: -1.5em auto -1.5em auto;
  width: 90%;
  margin: none auto none auto;
}

table.list.first {
  margin-top: 0;
}
table.list.last  {
  margin-bottom: 0;
}

table.list tr td {
  background-color: #fafafa;
  border: none;
  padding: 1em 0 1em 0;
  vertical-align: top;
  text-align: justify;
  height: 1px;
}

table.list tr td:first-child {
  width: 5%;
}


table.params {
  border-collapse: collapse;
  border-top: .5px solid #a6a6a6;
  border-bottom: .5px solid #a6a6a6;
  border-right: none;
  border-left: none;
  margin: 2em auto 2em auto;
  width: 30%;
}

table.params tr td {
  font-size: 14px;
  background-color: #fafafa;
  border-top: none;
  border-bottom: none;
  padding-top: .5em;
  padding-bottom: .5em;
  vertical-align: middle;
  text-align: center;
}

table.params tr td:first-child {
  text-align: left;
}

table.params tr:first-child td {
  border-bottom: .5px solid #cfcfcf;
}


</style>

The sampling distribution exists for any sample statistic (sample mean, median, standard deviation etc.) and it tells you how that sample statistic would be distributed if you computed for infinitely many random samples of size `\(N\)`. We will be using the sampling distribution of the sample mean `\(m\)`. 

In this section, we will simulate drawing a bunch of samples and compute the sample means `\(m\)`. We will use sample sizes `\(N=5,15,50\)`. All samples are drawn from the same population distribution with mean `\(\mu\)` and standard deviation `\(\sigma\)`. Because this is a simulation, we know what `\(\mu\)` and `\(\sigma\)` are (but their precise values don't matter). 

We start with a single random sample. In <b class="figreftext">FIGURE 3</b>, you can see a single sample for each sample size. The sample mean is marked by the colored square.

<!-- figure -->
<p class="figure">
  <img src="../plots/clt_onesample.svg">
</p>
<p class="caption">
  <b class="figreffig">FIGURE 3:</b>
  A single sample of size \(N\), drawn from the same population distribution with mean \(\mu\) and standard deviation \(\sigma\). The colored square and solid vertical line denote the sample mean \(m\). 
</p>

We can also draw a series of samples, and compute the sample mean for each of them. In <b class="figreftext">FIGURE 4</b>, you can see a series of 80 random samples for each sample size. Each "row" represents a single sample. Note the random variation of the sample means `\(m\)` (colored squares) about the population mean `\(\mu\)` (dotted vertical line). 

<!-- figure -->
<p class="figure">
  <img src="../plots/clt_manysamples.svg">
</p>
<p class="caption">
  <b class="figreffig">FIGURE 4:</b>
  A series of 80 samples of size \(N\), drawn from the same population distribution with mean \(\mu\) and standard deviation \(\sigma\). The colored squares denote the sample means \(m\). 
</p>

Now, what if we kept this infinitely often? We would get infinitely many sample means, which would form their own distribution

<!-- figure -->
<p class="figure">
  <img src="../plots/clt_sampdist.svg">
</p>
<p class="caption">
  <b class="figreffig">FIGURE 5:</b>
  A series of 80 samples of size \(N\), drawn from the same population distribution with mean \(\mu\) and standard deviation \(\sigma\). The colored squares denote the sample means \(m\). 
</p>
