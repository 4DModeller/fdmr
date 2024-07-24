---
title: '4DModeller: a spatio-temporal modelling package'
tags:
  - R
  - spatio-temporal modelling
  - Bayesian inference
  - INLA
  - inlabru
authors:
  - name: John M. Aiken
    orcid: 0000-0003-2258-3836
    affiliation: "1, 2"
  - name: Gareth Jones
    orcid: 0000-0003-4814-5156
    affiliation: 3
  - name: Xueqing Yin
    orcid: 0000-0003-1103-8939
    affiliation: 3
  - name: Anrijs K. Abele
    orcid: 0000-0002-6053-2629
    affiliation: 3
  - name: Christopher Woods
    orcid: 0000-0001-6563-9903
    affiliation: 3
  - name: Richard M. Westaway
    orcid: 0000-0001-6102-1540
    affiliation: 3
  - name: Jonathan L. Bamber
    orcid: 0000-0002-2280-2819
    corresponding: true
    affiliation: "3, 4"
affiliations:
  - name: Expert Analytics, Norway
    index: 1
  - name: University of Oslo, Norway
    index: 2
  - name: University of Bristol, UK
    index: 3
  - name: Technical University of Munich, Germany
    index: 4
date: 12 July 2024
bibliography: paper.bib
---

# Summary

4DModeller (`fdmr`) is a spatio-temporal modelling package capable of solving a wide range of large-scale space-time (i.e. four-dimensional) problems [@Yin:2023]. It is built around the inlabru framework which is a suite of R codes for fast efficient Bayesian inference [@Yuan:2017; @Bachl:2019;@Lindgren:2024a]. The `fdmr` package expands the inlabru framework to include specific applications of latent variable modelling for 4-D geophysical problems (e.g. ocean heat content, the Earth’s magnetic field, and global sea-level rise). `fdmr` also includes shiny apps that provide tools for data visualization, finite element mesh building and Bayesian hierarchical modelling based on an R package for Bayesian inference, inlabru, along with model evaluation and assessment. These shiny apps are designed to make the complex INLA framework [@Rue:2009] and associated concepts accessible to a wider scientific community, including users who have little to no previous experience using R. The tools are designed with new users in mind by leveraging their expertise with their data sets while minimizing the need to develop extensive code in R [@Aiken:2018; @Vygotsky:1978]. They allow users to interact with their data first using the intuitive knowledge of the modelling process (input data, create mesh, calculate statistical model), then auto-generating code that the users can build on.

This is extended through the Tutorial Driven Software Development practice [@Woods:2022]. This approach is designed to integrate subject matter experts into the code development cycle. It involves the identification of representative and instructive use cases, followed by tutorials that describe how the package could be used to solve them, and then finally code written and tested so that it behaved as described in the tutorials [@Woods:2022]. `fdmr` users have access to a set of domain-specific tutorials as vignettes in R Markdown notebooks; tutorials which are being added to as the user community grows.

The current development of the `fdmr` package supports a wide range of spatially heterogeneous and areal data, including in-situ point observations and satellite data. Examples of the former are ground station air pollution observations, rain gauge data, ocean buoy measurements, or GPS ground displacements. For areal data, the domain mesh is fixed (of regular or irregular shape) and partitioned into areal units (e.g. triangles) with well-defined boundaries. Examples of areal data are attributes collected by post code, satellite imagery, spatially gridded products such as climate re-analysis or land use classification.

Future package development efforts will focus on expanding its capabilities and broadening its applicability. Moreover, our team actively seeks interdisciplinary collaborations to further expand the modelling framework and tailor it to the specific needs of diverse disciplines.

# Statement of Need

The objective of the `fdmr` package is to provide researchers and practitioners with a straightforward and efficient solution for handling and analyzing different types of spatio-temporal data, enabling a comprehensive analysis of the underlying patterns, processes and trends.

`fdmr` reduces the computational complexity for scientific users of handling high-resolution, high-dimensional spatio-temporal data. Spatio-temporal data analysis is crucial in many research fields. However, modelling large-scale spatio-temporal data presents challenges such as high computational demands, complex correlation structures and the separation of mixed sources. Additionally, it requires expert domain knowledge to interpret model results. 4DModeller has been developed to address these issues; a robust and user-friendly R package designed to model spatio-temporal data within a Bayesian framework (inlabru) but without requiring users to have an in-depth knowledge of Bayesian statistics. Users have access to different solvers through the inlabru framework, such as the Integrated Nested Laplace Approximation (INLA) solver for approximate Bayesian inference and the stochastic partial differential equations (SPDE) method for defining a spatial model. Furthermore, `fdmr` provides intuitive and interactive visual analytics tools that facilitate the exploration of data patterns across both space and time. The goal is that `fdmr` will allow such tools to be used for process modelling (via Gaussian and Poisson processes) and for tasks such as latent process source separation [e.g. @Ziegler:2022] which both complements – and distinguishes it – from the existing example applications of inlabru [@Lindgren:2024b].

The work undertaken for 4DModeller extends the Bayesian hierarchical model developed as part of the ERC-funded GlobalMass grant (www.globalmass.eu) that advanced the use of space-time statistical inference to separate global sea level rise into its different sources [@CORDIS:2024]. The `fdmr` package has since been applied to several other use cases including COVID-19 transmission in England [@Yin:2024], streamflow in hydropower catchments in Norway, extreme rainfall in Nepal, changes in the Earth’s magnetic field and ocean heat content in the Pacific.

# Code Availability

The `fdmr` package and installation instructions are available from the 4DModeller GitHub repository (<https://4dmodeller.github.io/fdmr/index.html>) along with shiny apps and tutorials as vignettes in R Markdown notebooks.

# Acknowledgements

This work was supported by UK Research and Innovation grant EP/X022641/1. JLB was also supported by German Federal Ministry of Education and Research (BMBF) in the framework of the international future AI lab “AI4EO - Artificial Intelligence for Earth Observation: Reasoning, Uncertainties, Ethics, and Beyond” (grant number: 01DD20001). Code and tutorial development were aided by two hackathons (in Oslo in November 2023 and in Bristol in March 2024) which were only made possible by funding from the Research Council of Norway through the Svalbard Science Forum's funding program Svalbard Strategic Grant (project number: 344823).

# References
