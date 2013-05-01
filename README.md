[![Build Status](https://travis-ci.org/lewang/flx.png)](http://travis-ci.org/lewang/flx)

# This is a technology preview.

It's not ready to be used; treat with extreme caution.

[Screencast showing rationale and ido workflow][]

# ido support

Add this to your init file and *flx* match will be enabled for ido.

    (require 'flx-ido)
    (setq ido-enable-flex-matchint t
          flx-ido-use              t)



# helm support

Helm is not supported yet.  There is a demo showing how it could work, but I'm
still working through how to integrate it into helm.

The Helm demo shows the score of the top 20 matches.

# outstanding issues

## very large collections are slow

see `flx-ido-big-demo` for example.

There may be optimization opportunities in the matcher.
[Screencast showing rationale and ido workflow]: http://www.youtube.com/watch?v=_swuJ1RuMgk
