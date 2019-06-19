set -g ok_path "~/bin/Ok.hs"

function ok
    if test (count $argv) -gt 0
        eval (eval "$ok_path" $argv)
    else
        eval "$ok_path"
    end
end
