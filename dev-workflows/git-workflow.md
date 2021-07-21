The only rule of thumb: **Anything in the master branch is deployable**

## Workflow example

To work on some new feature/bugfix create a descriptively named branch off of master.

1. Create your working branch and jump to it

   On this branch I'm going to work with flume templates, so I've named it "flume-service"

   ```
   $ git checkout -b flume-service

   # This is shorthand for:
   #  git branch flume-service
   #  git checkout flume-service
   ```

2. Push your brand new branch to the remote server:

   ```
   $ git push -u origin flume-service
   ```

3. Push your commits to your named branches on the server constantly. Since the
  only thing we really have to worry about is master from a deployment
  standpoint, pushing to the server doesn’t mess anyone up or confuse things.

4. When you need feedback or help, or you think the branch is ready for merging,
  open a pull request.

5. If master branch has changes and you want work with this updates changes in
  your working branch you can merget the changes FROM master to your working branch:

```
$ git checkout flume-service
$ git merge master
```

## Tips

### git checkout for Remote Branches

The syntax for making git checkout "remote-ready" is rather easy: simply add the "--track" flag and the remote branch's ref like in the following example:

```
$ git checkout --track origin/newsletter
```

Branch newsletter set up to track remote branch newsletter from origin.
Switched to a new branch 'newsletter'
Based on the remote branch "origin/newsletter", we now have a new local branch named "newsletter".

Note that, by default, Git uses the same name for the local branch. Being a good convention, there's rarely the need to change this.

### Check diffs before commit within the $EDITOR
```
$ git commit -v
```

### TODO list about current works

A simple ‘git fetch’ will basically give you a TODO list of what every is currently working on:

```
$ git fetch --all
```
It also lets everyone see, by looking at the GitHub Branch List page, what
everyone else is working on so they can inspect them and see if they want to
help with something.

### Delete local branch and remote branch:

```
local branch: git branch -d fix_flume_dockerfile
remote branch: git push origin --delete fix_flume_dockerfile
```

### Remove all your local branches which are remotely deleted.
```
$ git fetch -p
```
you can set this automatically

```
git config fetch.prune true
or
git config --global fetch.prune true
```

### Get just one file from another branch
```
git checkout targetbranch
git checkout master -- filefrommaster
```
This command make a checkout of filefromaster file to targetbranch.

### Create local branch from an already created remote branch:
```
git checkout -b blueprints origin/blueprints
```
### Keeping a fork up to date with original repo

1. Clone your fork:

```
    git clone git@github.com:YOUR-USERNAME/YOUR-FORKED-REPO.git
```

2. Add remote from original repository in your forked repository: 

```
    cd into/cloned/fork-repo
    git remote add upstream git://github.com/ORIGINAL-DEV-USERNAME/REPO-YOU-FORKED-FROM.git
    git fetch upstream
```

3. Updating your fork from original repo to keep up with their changes:

```
    git pull upstream master
```

4. Push the local changes of your fork to your remote fork

```
git push origin master
git push --tags (this is whether the original repo added new tags or releases)
```

### Generate a git patch for a specific commit

```
git format-patch -1 <sha>

```

For generating the patches from the topmost commits from a specific sha1 hash:

```
git format-patch -<n> <SHA1>
```

The last 10 patches from head in a single patch file:

```
git format-patch -10 HEAD --stdout > 0001-last-10-commits.patch
```
# Hard, mixed, soft reset

When you modify a file in your repository, the change is initially unstaged. In
order to commit it, you must stage it—that is, add it to the index—using git
add. When you make a commit, the changes that are committed are those that have
been added to the index.

git reset changes, at minimum, where the current branch (HEAD) is pointing. The
difference between --mixed and --soft is whether or not your index is also
modified. So, if we're on branch  master with this series of commits:

- A - B - C (master)
HEADpoints to C and the index matches C.

When we run git reset --soft B, master (and thus HEAD) now points to B, but the
index still has the changes from C; git status will show them as staged. So if
we run git commit at this point, we'll get a new commit with the same changes
as C.

Okay, so starting from here again:

- A - B - C (master)
Now let's do git reset --mixed B. (Note: --mixed is the default option). Once
again, master and HEAD point to B, but this time the index is also modified to
match B. If we run git commit at this point, nothing will happen since the
index matches HEAD. We still have the changes in the working directory, but
since they're not in the index, git status shows them as unstaged. To commit
them, you would git add and then commit as usual.

And finally, --hard is the same as --mixed (it changes your HEAD and index),
except that --hard also modifies your working directory. If we're at C and run
git reset --hard B, then the changes added in C, as well as any uncommitted
changes you have, will be removed, and the files in your working copy will
match commit B. Since you can permanently lose changes this way, you should
always run git status before doing a hard reset to make sure your working
directory is clean or that you're okay with losing your uncommitted changes.

# Tags

Creating a tag from master branch for frezzing the version.

```
git checkout master
git tag -a v1.0.4 -m "Original stress version 1.0.4"
git tag -l
git push origin v1.0.4
```

# Git Ignore Tips

Check why a file is ignored:

```
git check-ignore --verbose gradle/wrapper/gradle-wrapper.jar
```
# git checkout for Remote Branches
```
git checkout --track origin/my-remote-branch
```


References:
* GitHub workflow: https://help.github.com/articles/what-is-a-good-git-workflow/
* Linux Kernel giteveryday: https://www.kernel.org/pub/software/scm/git/docs/giteveryday.html
* Kernel Hackers' Guide to Git: http://linux.yyz.us/git-howto.html
* Kernel Site Git User Manual: https://www.kernel.org/pub/software/scm/git/docs/user-manual.html

