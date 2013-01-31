A trivial Cabal package signing utility for use with Hackage
============================================================

## Signing packages

As a package author: first, generate your package distribution:

    chris@midnight:~/Projects/me/fay$ cabal sdist
    Distribution quality warnings:
    'ghc-options: -O2' is rarely needed. Check that it is giving a real benefit
    and not just imposing longer compile times on your users.
    Building source dist for fay-0.12.0.1...
    Preprocessing library fay-0.12.0.1...
    Preprocessing executable 'fay-tests' for fay-0.12.0.1...
    Preprocessing executable 'fay-docs' for fay-0.12.0.1...
    Preprocessing executable 'fay' for fay-0.12.0.1...
    Source tarball created: dist/fay-0.12.0.1.tar.gz

If you don't have GPG setup, it's easy. A trivial setup for newbies can be something like:

    $ gpg --gk # Generate a pub/priv key pair.

(On Ubuntu/Debian you can probably install with `sudo apt-get install pgp`.)

Next, sign your distribution:

    chris@midnight:~/Projects/me/fay$ cabal-sign sign dist/fay-0.12.0.1.tar.gz
    You need a passphrase to unlock the secret key for
    user: "Chris Done <chrisdone@gmail.com>"
    2048-bit RSA key, ID A2C5C589, created 2013-01-30

Now upload the signed version to Hackage:

    chris@midnight:~/Projects/me/fay$ cabal upload dist/fay-0.12.0.1.signed.tar.gz

## Verifying packages

As a user: Download the package:

    chris@midnight:~$ wget http://hackage.haskell.org/packages/archive/fay/0.12.0.1/fay-0.12.0.1.tar.gz

Verify it:

    chris@midnight:~$ cabal-sign verify fay-0.12.0.1.tar.gz
    gpg: Signature made Thu 31 Jan 2013 10:15:37 PM CET using RSA key ID A2C5C589
    gpg: Good signature from "Chris Done <chrisdone@gmail.com>"

## Sharing your key

You can share your key using the export feature:

    $ gpg --export > key.pub

And share it with people who want to trust your packages. Put it on
your web site, in your garden, tattoo it on your foot.

## Importing someone's key

This is also easy:

    $ gpg --import mary.pub

Now you can start verifying that packages are indeed from Mary.

## Other approaches

There're actually a bunch of nice key servers provided by Ubuntu and
supported with fancy GUIs and such, which might mitigate pain:
https://help.ubuntu.com/community/GnuPrivacyGuardHowto
