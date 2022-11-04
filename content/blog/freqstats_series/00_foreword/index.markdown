---
weight:       1
title:        Preface
subtitle:     The meaning of probability in frequentist statistics.   
date:         2022-10-01
draft:        false
excerpt:      |
  Preface to a series on the fundamentals of frequentist statistics.
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

/*---POP-UP FOOTNOTES---*/

/* hyperlink subclass fn,
   whose span is the pop-up footnote;
   default params below are when not hovering */
   
a.pop span {
  opacity: 0;
  pointer-events: none;
  width: fit-content;
  max-width: 10%; 
  font-size: 0.9em;
  text-align: left;
  position: absolute;
  background-color: #FEF6BB;
   -webkit-transition: all 0.5s ease;
   -moz-transition: all 0.5s ease;
   -o-transition: all 0.5s ease;
   transition: all 0.5s ease;
}
/* hyperlink subclass fn,
   whose span is the pop-up footnote;
   params below are specifically when hovering
*/
a.pop:hover span {
  opacity: 1;
  pointer-events: auto;
  margin-left: 1.5em;
  padding: .5em .8em .5em .8em;
  box-shadow: 0px 0px 15px grey;
   -webkit-transition: all 0.5s ease;
   -moz-transition: all 0.5s ease;
   -o-transition: all 0.5s ease;
   transition: all 0.5s ease;
}
/* subclass for figures*/
a.pop.fig span {
  background-color: #fafafa;
  padding: 1px 1px 1px 1px;
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

table.clean {
  border-collapse: collapse;
  border-top: .5px solid #a6a6a6;
  border-bottom: .5px solid #a6a6a6;
  border-left: none;
  border-right: none;
  margin-top: 3em;
  margin-bottom: 3em;
}

table.clean tr td {
  font-size: 13px;
  background-color: #fafafa;
  border-top: none;
  border-bottom: .5px solid #cfcfcf;
  padding-top: 1em;
  padding-bottom: 1em;
}

table.clean tr:firt-child td{
  border-top; none;
}

table.clean tr:last-child td{
  border-bottom: none;
}


</style>

Frequentist statistics is the most widely used framework for statistical inference. It is also the most widely *misused* framework. Both the scientific literature and textbooks are full of misinterpretations of p-values, confidence intervals, and so forth. How come? I wouldn't say that the frequentist framework is complicated. However, I would say that it is somewhat unintuitive. So, when teaching frequentist statistics, you should to spend some time to hammer home the fundamental principles. In my experience, statistics courses usually do the opposite: quickly brush over the fundamentals and then move on to "practical" stuff. So, I decided to write this blogpost-series about the fundamentals of frequentist statistics.

Enjoy the series---I hope you get something out of it. Feedback is always appreciated!
