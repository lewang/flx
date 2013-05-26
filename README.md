[![Build Status](https://travis-ci.org/lewang/flx.png)](http://travis-ci.org/lewang/flx)

# This is an early alpha

It's unlikely to crash your Emacs, but do expect to encounter bugs.


[Screencast showing rationale and ido workflow][]

# Memory Usage

The flx algorithm willingly sacrifices memory usage for speed.

For 10k file names, about 10 MB of memory will be used to speed up future
matching.  This memory is never released to keep the match speed fast.

So far with modern computers, this feels like a reasonable design decision.

It may change in future.


# GC Optimization

Emacs garbage collector is fairly primitive stop the world type.  GC time can
contribute significantly to the run-time of computation that allocates and
frees a lot of memory.

Consider:

    (defun uuid ()
      (format "%08x-%08x-%08x-%08x"
              (random (expt 16 4))
              (random (expt 16 4))
              (random (expt 16 4))
              (random (expt 16 4))))

    (benchmark-run 1
      (let ((cache (flx-make-filename-cache)))
        (dolist (i (number-sequence 0 10000))
          (flx-process-cache (uuid) cache))))
            ;;; ⇒ (0.899678 9 0.33650300000000044)

This means that roughly 30% of time is spent just doing garbage-collection.

`flx` can benefit significantly from garbage collection tuning.

By default Emacs will initiate GC every 0.76 MB allocated (`gc-cons-threshold`
== 800000).  If we increase this to 20 MB (`gc-cons-threshold` == 20000000)

We get:

    (benchmark-run 1
      (setq gc-cons-threshold 20000000)
      (let ((cache (flx-make-filename-cache)))
        (dolist (i (number-sequence 0 10000))
          (flx-process-cache (uuid) cache))))
        ;;; ⇒ (0.62035 1 0.05461100000000041)

So if you have a modern machine, I encourage you to add

    (setq gc-cons-threshold 20000000)

to your init file.

# ido support

Add this to your init file and *flx* match will be enabled for ido.

    (require 'flx-ido)
    (ido-mode 1)
    (ido-everywhere 1)
    (flx-ido-mode 1)
    ;; disable ido faces to see flx highlights.
    (setq ido-use-faces nil)



# helm support

Helm is not supported yet.  There is a demo showing how it could work, but I'm
still working through how to integrate it into helm.

The Helm demo shows the score of the top 20 matches.

# outstanding issues

## very large collections are slow

see `flx-ido-big-demo` for example.

There may be optimization opportunities in the matcher.
[Screencast showing rationale and ido workflow]: http://www.youtube.com/watch?v=_swuJ1RuMgk
