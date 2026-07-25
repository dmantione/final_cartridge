#!/bin/bash
#
# Execute this script to set up the git filter that will automatically clean the
# .fodt files before commit.
#
config filter.rmtags.clean 'manual/rmtags.sh'
git config filter.rmtags.smudge cat
