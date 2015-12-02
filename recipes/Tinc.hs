import Cook.Recipe
import Cook.Pacman
import Cook.Systemd

tinc :: Recipe ()
tinc = do
    installPackages ["tinc"]
    enableService "tinc"
