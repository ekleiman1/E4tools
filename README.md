
<!-- README.md is generated from README.Rmd. Please edit that file -->

# E4tools

Tested with TravisCI: [![Build Status](https://travis-ci.com/ekleiman1/E4tools.svg?branch=master)](https://travis-ci.com/ekleiman1/E4tools) 
E4tools is an early-release set of tools that you can use to automate
your workflow for analyzing EDA data that comes from the Empatica E4.
You can run these functions consecutively.

## What can you do now with E4tools?

There are currently four tools, all of which are part of the EDA
workflow (acc. workflow coming soon):

### The processing pipeline

**E4\_EDA\_Process.part1.ExtractRawEDA**, which allows you extract and
filter EDA data. It will output raw data, filtered data (using
user-specified high and low pass filters + a butterworth filter), and
filtered + feature-scaled (\[0,1\]) data. It will also provide summary
data at the participant and session level.

**E4\_EDA\_Process.part2.ExtractButtonPresses**, which allows you
extract button pressess and remove pressess that are within a certain
number of minutes before the end of a session or that are too close to
another button press.

**E4\_EDA\_Process.part3.MatchPressesToEDA**, which allows you to
extract the data that are within X minutes before and/or after a button
press.

**E4\_EDA\_Process.part4.BinMatchedEDA**, which allows you to bin the
data that has been matched to the button pressess (from step 3).

## Frequently Asked Questions

### How do I install E4tools?

`install.packages("E4tools")`

### How do I structure my data for E4 tools?

Your files should be structured such that ZIP files should be grouped by
participant – that is, each ZIP file (i.e., what you downloaded from
Empatica Connect) is in a folder whose name is the participant ID.

## Funding

This package was created to support data from projects R34MH113757-01A1
and R21MH115293 to Evan M. Kleiman.
