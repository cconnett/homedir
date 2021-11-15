# Make symbolic links for the files in ~/homedir; make them in ~.

pushd ~/homedir > /dev/null

for filename in $(ls -AB1 . | egrep '^\.'); do
  [ -f "$filename" ] && [ ! -e "$HOME/$filename" ] && ln -s "$HOME/homedir/$filename" "$HOME/$filename" && echo "Linked $filename"
done

# Allowlist two directories.
for filename in .emacs.d/site-lisp .xmonad; do
  [ -d "$filename" ] && [ ! -e "$HOME/$filename" ] && ln -s "$HOME/homedir/$filename" "$HOME/$filename" && echo "Linked $filename"
done

popd > /dev/null
