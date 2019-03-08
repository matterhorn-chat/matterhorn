
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

Design Philosophy
-----------------

We aim to take care of the following concerns when adding new features:

 * We want Matterhorn to be responsive.
 * We want Matterhorn to use available system resources when possible.
 * We want Matterhorn's system resource usage to be user-constrainable.
 * We want Matterhorn to default to using only as much system resources
   as necessary.
 * We want to avoid exposing or leaking Haskell internals (such as RTS
   options) to the end user. Instead, we want to use our own idioms.
 * We want configuration to be as clear, simple, and future-proof as
   possible. (Avoid: confusing, complex, error-prone, future-brittle.)

Commit Hygiene
--------------

Before committing or merging, strive for a `-Wall`-clean build.

Each commit should ideally make one logical, self-contained change. For
example, by this reasoning a commit should add one new feature, fix one
bug, or do one incremental refactoring step. A single commit maintains
a successful build and working software whenever possible. Commits that
break things that are intended to be fixed at a much later date should
be avoided if possible. When unavoidable they should be indicated very
clearly by their commit message.

Commits should also be concise so that if something has to be reverted
or updated it's easy to focus on the origin of the change in its
minimum form. Commits should not be squashed (no mega-commits, please).

A commit should have a commit message describing the purpose of the
change, the salient code elements to bring to the reader's attention,
any motivations or context for the change, any caveats, and any
intentions about the future. A good commit message template is:

```
<scope>: <concise one-line high-level description>

<paragraph describing in greater detail all of the above>
```

where `scope` is the name of a module, file, function, etc. that plays
the most central role in the change.

We want commit messages to be a good reference for someone (maybe you!)
wanting to understand a code change (maybe your own!) in the future.
Capturing reasoning and other context makes the commit history useful in
this way.

One way to support this workflow is to always use hunk prompting with
git:

```
$ git add -p
```

and only add hunks relevant to your logical change and split large hunks
into small ones with the `s` choice to ensure that incidental unrelated
changes don't make it into the commit. You should only ever use `git
add <file>` if you are *absolutely sure* the entire file belongs in the
commit.

Aesthetics
----------

When working with the code, we do not have a "style guide" and do not
want to enumerate all the different ways that the code can or should
be formatted.  We also recognize that the specific formatting
decisions are often affected by editor behaviors and individual typing
habits.

The ultimate goal is readability and so in general, we recommend that
you loosely follow existing formatting by default (eyes like to see
familiar patterns), but feel free to implement specific practices where
these match the editor tooling and or your personal aesthetics.

The following are some of the aesthetic choices we've made; these are
not hard rules but help to explain the current overall appearance:

  * Imports are usually in alphabetical order and separated into 4-5
    sections from the most global/generic to the most local/specific.
    Because there can be a number of sections, we separate the imports
    from the module body with two lines to help visually locate this
    point.  See Events.hs as an example.
    
  * We have collected the most common Prelude elements into a local
    file included by all modules (and therefore hiding the normal
    Prelude).  This helps to reduce the number of overall imports, and
    we keep this as the first import section to draw attention to the
    fact that the normal Prelude is not in context by default.
    
  * We prefer not to mix the `.` and `$` operators on the same line:

    ```haskell
    let foo = func . chain . call $ parse $ input
    let foo = func $ chain $ call $ parse $ input  -- preferred
    ```

  * Top-level functions have haddock documentation to help describe
    what the function is used for (and sometimes ways in which it
    shouldn't be used).

  * Modules with more than a few exports should have an explicit export
    list to help us identify dead code and capture API usage
    expectations.
    
Branching
---------

 * The `master` branch should be stable enough to be release-ready at
   all times. This promise helps us be agile in the event of a need
   to do an unplanned release and it ensures that users who clone the
   repository will get a working build by default.

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
   questions and requests for changes. We do this to ensure that other
   team members are aware of the ramifications of the work, to provide
   an opportunity for feedback, and to get fresh eyes on the work to
   spot problems that won't be evident to its author.

 * Bug fixes and other small, uncontroversial changes can be committed
   directly to `master` and merged back into `develop`. This ensures
   that bug fixes are not held up by other development work in case a
   bugfix release is desired.

Compatibility
-------------

Matterhorn is primarily distributed to users as binaries that have
been built for that purpose; the primary compatibility support goal is
the set of operating systems binaries are built for.

From an operating system perspective, the "current stable release"
versions of those operating systems are the primary targets.  We
support many of the more popular operating systems, including: CentOS,
Fedoria, Ubuntu, and MacOS.

Those wishing to use Matterhorn on a different operating system (that
is not compatible with one of the pre-built binaries) or work with the
code directly by building from source will need to obtain the source
directly and follow the build instructions described in the README
document. The GHC compiler version documented in the build
instructions is the primary supported version.

Continuous Integration (CI) systems (e.g. Travis and/or Hydra) are
used for determining the viability of builds for different GHC
compiler versions, and in some cases on different operating systems.
The general intent is to support 3 GHC compiler versions (the current
and the two most previous versions) to allow flexibility for developer
configurations, but we cannot commit to supporting any version other
than the one documented in the README.

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

Design
------

We would like issues not to linger too long unaddressed, because then
the meaning of "open ticket" is ambiguous to us and to end users: is it
planned? Is it well-understood? It's unclear.

But we can only tackle so many things, and sometimes we don't even know
enough about what's involved in a task to know its level of effort. So
in the mean time, before we can investigate, the ticket lingers.

Once we're learning what is involved, using the ticket as a place to
hash out ideas or collect context is annoying because using ticket
comments for that isn't effective.

To address these concerns:

 * Whenever we have some high-level thing we want to implement, instead
   of creating a placeholder ticket, make a page on the project wiki at

   https://github.com/matterhorn-chat/matterhorn/wiki

   On this page we'll collaboratively hash out design ideas, collect
   context and research, and share approaches. These documents represent
   a "staging ground" for new ideas.

 * Once enough context has been gathered to support moving forward on
   the feature, create concrete tickets from the context. It's also
   possible we decide *not* to move forward, in which case the rationale
   for aborting can be clearly captured on the wiki. This way, external
   users can see why we decided not to implement something.

 * When external users create tickets for high-level features that
   cannot be implemented immediately but that we agree deserve
   consideration and need further investigation, close the tickets
   by referring to the newly-created context wiki page where we'll
   be hashing out the details. This means that we strive to ensure that
   open tickets are by definition always workable and fleshed-out.

 * This way, when external users stop by to ask about features, we can
   point them at those pages to make it easier to 1) share the developed
   context, 2) make it clear what is missing in case they want to help,
   and 3) keep a record of design decisions for posterity even after the
   feature is implemented.
