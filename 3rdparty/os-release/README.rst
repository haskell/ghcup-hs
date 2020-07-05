##########
os-release
##########

http://www.freedesktop.org/software/systemd/man/os-release.html

Usage
#####

.. code-block:: haskell

  import System.OsRelease
  main = readOs >>= print

.. code-block::

  Right (fromList [(OsReleaseKey "ANSI_COLOR",OsReleaseValue "1;32"),(OsReleaseKey "BUG_REPORT_URL",OsReleaseValue "https://bugs.gentoo.org/"),(OsReleaseKey "HOME_URL",OsReleaseValue "http://www.gentoo.org/"),(OsReleaseKey "ID",OsReleaseValue "gentoo"),(OsReleaseKey "NAME",OsReleaseValue "Gentoo"),(OsReleaseKey "PRETTY_NAME",OsReleaseValue "Gentoo/Linux"),(OsReleaseKey "SUPPORT_URL",OsReleaseValue "http://www.gentoo.org/main/en/support.xml")])
