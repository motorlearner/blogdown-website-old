---
weight:       2
title:        In the Long Run
subtitle:     The meaning of probability in frequentist statistics.   
date:         2022-10-01
draft:        false
excerpt:      |
  This is part 1 of a series on the fundamentals of frequentist statistics.
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
  border-left: 0.7em solid #cccccc;
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

p.foreword{
  margin-bottom: 2em;
  padding-bottom: 1em;
  border-bottom: 1.5px solid #b5b3b3;
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

/* list */
li{
  font-size: 14px;
  text-align: justify;
  padding-left: 1em;
}

/* headings */
h1{
  font-size: 11px;
}

h2{
  font-size: 6px;
}

/*---POP-UP FOOTNOTES---*/

/* hyperlink subclass fn,
   whose span is the pop-up footnote;
   default params below are when not hovering
*/
a.fn span {
  opacity: 0;
  pointer-events: none;
  width: 30%;
  font-size: 0.9em;
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
  margin-top: 2ex;
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

</style>

<p class="foreword">
This is part 1 of a series on the fundamentals of frequentist statistics. Hope you enjoy, feedback is always appreciated!
</p>

Statistical inference is based on probability theory. Probability has a clear definition in the abstract mathematical world: it is a number between 0 and 1, and the probability that any one of multiple mutually exclusive events occurs is the sum of their individual probabilities. But if we apply probability to our real world, what exactly does it quantify there?

The answer depends on the statistical framework. In the *frequentist* framework, probabilities refer to hypothetical long run *frequencies*. For example, when flipping a coin, the probability of heads is 0.5. That means, if we repeatedly flipped a coin, the frequency of heads would tend to 0.5 as the number of coin flips approaches infinity. A simulated version of this is shown in <b class="figreftext"> FIGURE 1</b>. 

<!-- figure: -->
<p class="figure">
  <img src="../plots/coin.svg" />
</p>
<p class="caption">
  <b class="figreffig">FIGURE 1:</b>
  Three simulated coins, each flipped 3000 times. The x-axis shows the number of flips. The y-axis shows the frequency of heads for each coin. After the first flip, the frequency of heads is either 0 or 1. As the number of flips grows, the frequency of heads tends to 0.5. 
</p>

More generally, the probability that a process produces a certain outcome is the frequency at which that outcome is observed *in the long run*, i.e. *if the process is repeated infinitely often*. 



Blablabla <a class="fn" href="#whatispop-foot" aria-describedby="footnote-label" id="whatispop-text"> <span>An interesting and informative comment appears here, as if by magic. What if there is more text though. Can it go on and on and on? For how long? Even more text. And more. More more more. What if there is more text though. Can it go on and on and on? For how long? Even more text. And more. More more more. What if there is more text though. Can it go on and on and on? For how long? Even more text. And more. More more more more.</span></a>.

<!-- footnotes -->
<footer>
<h3 class="visually-hidden" id="footnote-label"> Footnotes </h2>
<ol>

<li id="whatispop-foot">
In surveys, participants may be included at random, in which case the population refers to all people that could have been drawn. In trials, participants are typically not included at random but are randomly allocated to group, in which case the population refers to all possible allocations of the included participants. It is pretty rare that participants are both included and allocated at random, but in this case the population would refer to all possible allocations of all people that could have been included. 
<a href="#whatispop-text" aria-label="Back to content"> ↩ </a>
</li>

</ol>
</footer>
