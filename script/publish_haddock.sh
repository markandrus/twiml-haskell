#!/bin/bash
# Description: This script is run by Travis CI on successful builds in order to
# update the repo's GitHub pages with Haddock documentation. 
set -e

user='markandrus'
repo='twiml-haskell'
package='twiml'

if [[ "${TRAVIS_REPO_SLUG}"  == "${user}/${repo}" \
   && "${TRAVIS_PULL_REQUEST}" == 'false' \
   && "${TRAVIS_BRANCH}" == 'master' ]]
then

  echo "Generating Haddock documentation..."
  cabal haddock
  cp -R dist/doc/html/${package} ${HOME}/doc

  echo "Cloning repo..."
  cd ${HOME}
  git config --global user.email 'travis@travis-ci.org'
  git config --global user.name 'travis-ci'
  git clone --quiet --branch=gh-pages ${repo} gh-pages >/dev/null
  
  echo "Updating repo..."
  cd gh-pages
  rm -rf *
  mv ../doc/* gh-pages
  git add -f .
  git commit -m "Updating Haddock documentation (${TRAVIS_BUILD_NUMBER})"
  git push -fq origin gh-pages >/dev/null
  
  echo "Published Haddock documentation."

fi
