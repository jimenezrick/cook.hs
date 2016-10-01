# Forget shell script!

Simple/effective configuration management + containerization:

Use Haskell as an expressive DSL to replace shell script and do all the
heavy lifting with systemd.

## Showcase

- Let's create an Arch rootfs:

[![asciicast](https://asciinema.org/a/dfj33fmvqbm2d0smvugyts70c.png)](https://asciinema.org/a/dfj33fmvqbm2d0smvugyts70c)

- We are going to set up a [cjdns](https://github.com/cjdelisle/cjdns)
  node fully configured inside:

[![asciicast](https://asciinema.org/a/4wsbrzcvra543edn88o0xl2br.png)](https://asciinema.org/a/4wsbrzcvra543edn88o0xl2br)

- The image of the container can now be packed and deployed in another
  machine with systemd using `machinectl`:

[![asciicast](https://asciinema.org/a/9f3ip3h0z1bsyexjpkvymzhn5.png)](https://asciinema.org/a/9f3ip3h0z1bsyexjpkvymzhn5)
