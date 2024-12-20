[This page is still under construction]
# Foreword

## Aim of processing variables
Processing intensity and distance variables  
- help overcome technical variability between images, enabling effective comparisons across replicates and treatments
- reveals relative fractions that provide meaningful insights

## Reference Object for Normalization
The reference object is the Surface object "Nucleus". 

In the future, DataViz will be customizable for this reference object

Workaround if your images do not correspond to Nucleus images:

&nbsp;&nbsp;Create a main Surface delimiting your region of interest and name it "Nucleus" 

## Application Examples
Based on nuclear and chromatin biology studies - transferable to any other cellular studies
- the Relative Heterochromatin Fraction (RFF) will be the Group Intensity Sum Relative to Nucleus, for the chromatin (DNA) staining channel
- local chromatin compaction can be infered from the Intensity Mean. When Normalized by Group, it will provide information on the variability of chromatin compaction within he segmented nuclear regions (speckles, bodies, chromocenters..)
- the fraction of a protein clustering in discrete regions (and segmented) can be measured by the Group Intensity Sum Relative to Nucleus
- the enrichment of a chromatin protein relative to DNA within speckles, or nuclear domains, can be measured by the Normalized Intensity Sum Ratio (of channels to be defined)
- ....
  
(more examples to come)

# Intensity Variables
## Aim 
Intensity Sum, Intensity Mean and Intensity Standard Deviation (Intensity StdDev) provides different information
- Intensity Sum gives the total amount of signal within one object 
- Intensity Mean informs on the density of signal within one object
- Intensity StdDev is a proxy for heterogeneity of signal distribution within an object
  
Normalization or expression of a ratio allows comparison between image replicates, and to infer meaningful information on the distribution of the signal (labelled component of interest)

## Intensity Normalized per Nucleus
### Intensity Sum Normalized per Nucleus
This variable requires a reference object "Nucleus"

For each image, each object, each channel, the Intensity Sum of the object is divided by the Intensity Sum in the Surface “Nucleus”

This variable expresses the fraction of the signal in one object relative to the whole nucleus. 

**Application example:** Chromatin staining shows densely staining regions (chromocenters, heterochromatin) that can be segmented. The Intensity Sum Normalized per Nucleus, expresses how much chromatin is present in each chromocenter.  This would be the Relative Chromatin Fraction per Chromocenter

###  Intensity Mean Normalized per Nucleus
This variable requires a reference object "Nucleus"

For each image, each object, each channel, the Intensity Mean of the object is divided by the Intensity Mean in the Surface “Nucleus”

**Application example:** Building on the example of Chromocenters,  the Intensity Mean Normalized per Nucleus, expresses how the density of chromatin per chromocenter relates to the overall density in the nucleus

###  Intensity StdDev Normalized per Nucleus
This variable requires a reference object "Nucleus"

For each image, each object, each channel, the Intensity StdDev of the object is divided by the Intensity StdDev in the Surface “Nucleus”

**Application example:** Building on the example of Chromocenters,  the Intensity StdDev Normalized per Nucleus, allows to measure whether chromatin is more or less heterogenous per chromocenter compared to the overall distribution in the Nucleus

## Intensity Normalized by Group
### Intensity Sum Normalized by Group
This variable requires the variable “Group Intensity Sum” 

For each object, each channel, the Intensity Sum is divided by the Group Intensity Sum.

This variable expresses the relative fraction of signal in one object within its group

**Application example:** The signal of a GFP-tagged protein X is distributed throughout the nucleus, with some fraction diffused and another appearing as densely staining bodies that can be segmented. The Intensity Sum Normalized by Group quantifies the relative fraction of this protein per body, enabling comparisons of its distribution under different conditions. For instance, if one condition produces fewer but brighter bodies, the Intensity Sum Normalized per Group will be higher than in the control condition. This reflects the "Relative Fraction of Protein X in Nuclear Bodies".

### Intensity Mean Normalized by Group
This variable requires the variable “Group Intensity Mean” 

For each object, each channel, the Intensity Mean is divided by the Group Intensity Mean.

This variable expresses the relative density of signal in one object within its group

**Application example:** a GFP tagged protein shows brighter nuclear bodies under condition A compared to condition B. This difference can be quantified by comparing the Intensity Means Normalized by Group in this nuclear bodies, which can be called “Relative density of protein X in nuclear bodies”

## Group Intensity variables
### Group Intensity Sum
For a given group of objects carrying the same name, the Intensity Sums are added up (Sum of Intensity Sum)

**Application:** the Group Intensity Sum reports on the total amount of signal present in the group of segmented objects. This variable is most useful when used for normalization (Intensity Normalized by Group) or expressed relative to the signal in the reference surface (Group Intensity relative to Nucleus)

### Group Intensity Mean
For a given group of objects carrying the same name, the Intensity Means are averaged (Mean of Intensity Mean)

**Application:** this variable is most useful to calculate the follow-up variables, Intensity Mean Normalized by Group and Group Intensity Mean Relative to Nucleus

### Group Intensity Sum Relative to Nucleus
This variable requires the “Group Intensity Sum” and a reference surface, “Nucleus”

For each object, each channel, the  Group Intensity Sum is divided by the Intensity Sum in the Nucleus

This variable expresses the relative fraction of signal in a group of segmented objects relative to the total signal in the reference surface, “Nucleus”

**Application example:** This variable can be used to calculate the Relative Heterochromatin Fraction (RHF), or , similarly, any Relative Fraction of a signal distributed both as discrete regions (segmented) and as diffuse signal (not segmented)

### Group Intensity Mean Relative to Nucleus
This variable requires the “Group Intensity Mean” and a reference surface, “Nucleus”

For each object, each channel, the  Group Intensity Mean is divided by the Intensity Mean in the Nucleus

This variable expresses the relative density of the signal in a group of segmented objects relative to the density overall in the reference surface, “Nucleus”

**Application example:** Chromocenters are decondensed in condition A compared to condition B. This is quantified by a lower Group Intensity Mean Relative to Nucleus, indicating the “Relative Chromatin Density in CC”.

## Channel ratios:  
The ratio is made between two channels (ch) defined by the user (a, b in example below), per object

### Ratio of Intensity Sum
For each object the Intensity Sum in [ch.a] is divided by the Intensity Sum in [ch.b]

### Ratio of Intensity Sum Normalized per Nucleus
For each object, the ratio is made between the values: Intensity Sum Normalized per Nucleus

### Ratio of Intensity Sum Normalized by Group
For each object, the ratio is made between the values: Intensity Sum Normalized by Group

# Distance variables
## Distance Normalized per Nucleus
**Requirement:** spot object called ‘Nucleus Center Of Mass’ (created during image segmentation)

For each image, the shortest distance of the spot = ‘Nucleus Center Of Mass’ to the surface = ‘Nucleus’ is taken as a reference distance

For each object, the ‘spot-to-surface’ shortest distance is divided by the reference distance

Note that this normalization is interesting only if the nucleus is near spherical

**In future:** this distance normalization will be customizable to other variables (surface-to-surface) and to other metrics (eg Nucleus diameter X,Y,Z ; Area)

# Shape variables
## Volume Relative to Nucleus 
Per object, the volume is divided by the volume of the Surface=Nucleus in the same image

## Volume Relative to  Group 
This variable requires the *Group Volume" variable

Per object, the Volume is divided by the Group Volume in the same image

## Group Volume 
Per image, the volumes of objects belonging to the same group, same image, are summed up

## Group Volume Relative to Nucleus 
This variable requires the *Group Volume" variable

The Group Volume is divided by the volume of the Surface=Nucleus in the same image

## Surface-to-Volume Ratio 
Per object, the surface is divided by the volume

## Group Surface
The surface of objects belonging to the same group, same image, are summed up

## Group Surface-to-Volume Ratio  
This variable requires the variables "Group Surface" and "Group Volume"

Per group of object, the "Group Surface" is divided by the "Group Volume"

# Object counts
## Object count per Group
