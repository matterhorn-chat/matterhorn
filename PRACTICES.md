
Matterhorn Project Practices
============================

This document captures some important project practices that we employ.
If you want to contribute to Matterhorn, for best results, please follow
these practices.

Many of the practices described below are really about serving some
overall concerns:

 * We want to provide a working program to people who want to use it,
 * We want to manage inevitable software evolution, and
 * We want to reduce waste through good team coordination.

Branching
---------

 * The `master` branch should be stable enough to be release-ready at
   all times. This promise helps us be agile in the event of a need to
   do an unplanned release.

 * The `develop` branch is where development occurs.

 * New feature development begins and stays on a new feature branch,
   typically named `feature/FEATURENAME`, until it is declared stable
   enough to be merged. Once more testing of a feature is needed,
   especially in concert with other work, it should be merged to
   `develop`.

 * Once `develop` has been deemed stable enough to release, it can be
   merged to `master`.

 * When a feature branch implementation is complete, before merging to
   `develop` we do a code review that is appropriate to the level of
   complexity and risk of the work being merged, which may involve an
   in-person code tour but typically involves just commit review with
   questions and requests for changes.

 * Bug fixes and other small, uncontroversial changes can be committed
   directly to `master` and merged back into `develop`. This ensures
   that bug fixes are not held up by other development work in case a
   bugfix release is desired.

Issue Tracking
--------------

 * We use the GitHub issue tracker for user bug reports and feature
   requests, and to capture our own intentions about planned and
   potential work.

 * We use milestones to group tickets for big efforts, such as server
   compatibility releases or planning cycles.

 * When creating issues, we try to make issues as actionable as possible
   when the scope and details are well-understood, but this isn't always
   possible. If we can't be concrete about what a ticket entails, we
   assign the label `high-level` to indicate that more team discussion
   or context-gathering will be required before a concrete plan can be
   made. We also use the label `high-level` to indicate any ticket with
   insufficient detail that needs further discussion before we can
   tackle it.

Workflow
--------

 * When planning big efforts, we strive to get team alignment on the
   scope of planned efforts. Creation of a ticket does not necessarily
   mean that we have gotten team alignment or settled on scope. The
   appropriate scope will be a function of time and availability,
   pragmatism, feature parity with upstream, motivation, etc.

 * When doing refactoring that may be risky or touch a lot of the
   program, we strive to break the job down into a sequence of
   incremental changes that each produce a working program and
   ultimately get us where we want to be.

 * When planning new work, we strive to get input from the team before
   the work begins, rather than after it is completed, to reduce waste
   and to ensure that the work is well-informed. The bigger or more
   impactful the change, the more important this is - and it applies to
   work done in spare time as well as at work.

 * If resources allow, consider pair programming as a means of tackling
   bigger, cross-cutting problems. Although it costs more in terms of
   person-hours, it can be a very effective technique for producing
   better designs with fewer bugs and more mind-sharing.

 * For experimental work or new features, consider "prototyping" or
   "throwaway coding," in which the first implementation of a new
   feature is *intended* to be discarded upon completion. Rather than
   producing code, the result of this is the learning that occurs
   when exploring a design. Then, having learned, one can embark on a
   better-informed implementation.
