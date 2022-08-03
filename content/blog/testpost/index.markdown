---
# text
title:      "The testpost"
subtitle:   "A Testpost"
excerpt:    "Ignore this. It's just a testpost."

# metadata (NB bottom will show "see also: all posts with same tag")
author:     "Aslan B."
date:       2022-04-01
categories: 
- test
tags:
- test

# other
layout:     single
draft:      false
---

This is simply a template to remind myself of all the cool features that you can put
into blogposts. 

### Using Panelsets

These kind of things are pretty cool, they kind of work like browser tabs:

{{< panelset class="something" >}}
{{< panel name="Maths" >}}
  `\(p = P(T \ge t; M)\)`
{{< /panel >}}
{{< panel name="Meaning" >}}
  The p-value is the probability of observing a test-statistic at least as large
  as we have observed, given a certain statistical model (which includes the Null
  hypothesis).
{{< /panel >}}
{{< panel name="Wrong" >}}
  The p-value is the probability that the null hypothesis true.
{{< /panel >}}
{{< /panelset  >}}

### Tables

Markdown allows you to build tables. This is what they look like:

| Left Aligned | Mid Aligned | Right Aligned |
| :----------- | :---------: | ------------: |
| First        | First       | First         |
| Second       | Second      | Second        |

### Text Stuff

Can use inline maths like `\(1+1=2\)` or put it into a block. Note that there are some 
differences to LATEX. For example, manually adding a tag adds the tag to the whole 
block, it appears. 

$$
`\begin{align}
a^2+b^2 &= c^2  \\
\sqrt{(a^2+b^2)} &= c
\tag{1}
\end{align}`
$$

There is also the usual stuff such as 
> Blockquotes

or a horizontal line (maybe useful to put around figures, with figure caption inside.)
____

Here is a picture, loaded from the plots folder. 

![alt text](plots/pic.svg "Title")
