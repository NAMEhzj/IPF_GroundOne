# IPF_GroundOne
a more general version of Interval IPF (see IPF_GroundZero by Eva Richter) that is also a bit tidier

this repository contains:
  - an interval type and operations on it (in folder Interval)
  - the alrorithm IPF for any number of dimensions over an abstract type as instance
      of a typeclass (in IPFCore)
  - an instance for said typeclass for the Interval type (in IPFNum)
  - a generator for random samples (in SampleGen)
  - functions to display results of anIPF Computation as a Graph 
      using LaTeX TikZPicture (in VisualData & Graphics)
