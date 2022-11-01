---
weight:       3
title:        Rejecting the Incompatible
subtitle:     The logical argument underlying frequentist statistics.   
date:         2022-10-01
draft:        false
excerpt:      |
  Part 1 of a series on the fundamentals of frequentist statistics.
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
   
a.fn span {
  opacity: 0;
  pointer-events: none;
  width: fit-content;
  max-width: 60%; 
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
a.fn:hover span {
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

Forget about statistics for a moment. Imagine we are at a scientific conference. We are here because we want talk to a scientist called Rick. However, we don't know what Rick looks like---all we know is that he is always wearing a lab coat. We look carefully, but no one at the conference is wearing a lab coat.

1. If Rick is at the conference, then someone will be wearing a lab coat.  
2. No one is wearing a lab coat.  
3. Therefore, Rick is not at the conference. 

We conclude that Rick is not at the conference. Now, let's say we do see someone wearing a lab coat.

1. If Rick is at the conference, then someone will be wearing a lab coat. 
2. Someone is wearing a lab coat. 
3. -- 

We cannot conclude anything. Sure, we might have seen Rick in his lab coat---or Rick wasn't there at all and we just saw someone else wearing a lab coat.

## Modus Tollens

The logical argument that we have just used is called *modus tollens* or "denying the consequent". We start with a premise of the form &nbsp; "if `\(T\)` then `\(O\)`", &nbsp; i.e. a model/assumption that tells us what observation (`$O$`) to expect under a given possible truth (`$T$`). Next, we make an observation. Finally, if our observation is not what we would expect under the possible truth, we reject the possible truth.

<table class="clean">
  <tr>
    <td style="width:34%">
      <i> 3. Model/Assumption </i>
    </td>
    <td colspan="2">
      If \(T\) (= Rick is at the conference),<br>then \(O\) (= someone will be wearing a lab coat).
    </td>
  </tr>
  <tr>
    <td>
      <i> 4. Observation </i>
    </td>
    <td style="width:33%">
      \(O\).
    </td>
    <td>
      Not \(O\).
    </td>
  </tr>
  <tr>
    <td>
      <i> 5. Conclusion </i>
    </td>
    <td>
      &mdash;
    </td>
    <td>
      Therefore not \(T\).
    </td>
  </tr>
</table>

This is basically the argument used in frequentist statistics. However, in the table above, a few steps are left implicit (hence why the table starts with step 3). The table below shows the exact same argument, but with all steps spelled out (thus starting with step 1).

<table class="clean">
  <colgroup>
        <col width="5%"><col width="5%"><col width="5%"><col width="5%">
        <col width="5%"><col width="5%"><col width="5%"><col width="5%">
        <col width="5%"><col width="5%"><col width="5%"><col width="5%">
        <col width="5%"><col width="5%"><col width="5%"><col width="5%">
        <col width="5%"><col width="5%"><col width="5%"><col width="5%">
    </colgroup>
  <tr>
    <td colspan=7>
      <i> 1. List all possible truths. </i> <br>
      \(T_1, T_2, T_3 ... \)
    </td>
    <td colspan=13>
      \(T_1\): Rick is at the conference. <br>
      \(T_2\): Rick is not at the conference.
    </td>
  </tr>
  <tr>
    <td colspan=7>
      <i> 2. List all possible observations. </i> <br>
      \(O_1, O_2, O_3 ... \)
    </td>
    <td colspan=13>
      \(O_1\): Someone is wearing a lab coat. <br>
      \(O_2\): No one is wearing a lab coat.
    </td>
  </tr>
  <tr>
    <td colspan=7>
      <i> 3. For each possible truth, define which 
      observations are compatible and which are incompatible (Model/Assumption). </i> <br>
      <img src="compatible1.svg">
    </td>
    <td colspan=9>
      If \(T_1\), then \(O_1\). <br> \(\Rightarrow\) \(T_1\) compatible with \(O_1\). <br> \(\Rightarrow\) \(T_1\) incompatible with everything else, i.e. \(O_2\). <br><br>
      If \(T_2\), then \(O_1\) or \(O_2\). <br> \(\Rightarrow\) \(T_2\) compatible with \(O_1\) and \(O_2\). <br> \(\Rightarrow\) \(T_2\) incompatible with everything else, i.e. nothing.
    </td>
    <td colspan=4; style="vertical-align: middle;">
      <img src="compatible2.svg">
    </td>
  </tr>
  <tr>
    <td colspan=7>
      <i> 4. Make an observation. </i>
    </td>
    <td colspan=7>
      \(O_1\): Someone is wearing a lab coat.
    </td>
    <td colspan=6>
      \(O_2\): No one is wearing a lab coat.
    </td>
  </tr>
  <tr>
    <td colspan=7>
      <i> 5. Reject all possible truths that are incompatible with the observation. </i>
    </td>
    <td colspan=7>
      Reject nothing. 
    </td>
    <td colspan=6>
      Reject \(T_1\). 
    </td>
  </tr>
  <tr>
    <td colspan=7>
      <i> 6. The actual truth is somewhere among the remaining possible truths (which
      are compatible with the observation). </i>
    </td>
    <td colspan=7>
      \(T_1\) or \(T_2\): Rick is or is not at the conference.
    </td>
    <td colspan=6>
      \(T_2\): Rick is not at the conference.
    </td>
  </tr>
</table>

Two key points are 3 and 5. First, everything in frequentist statistics is based on if-then statements. Literally all the quantities (p, error rates, etc.) are based on if-then statements. Second, we can only ever reject the incompatible. We can only conclude something if we have rejected everything else. 

On to the probailistic version. Sneak peak at false rejection rate. 
