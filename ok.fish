set OK_PATH "~/bin/Ok.hs"

function ok
    if test (count $argv) -gt 0
        eval (eval "$OK_PATH" $argv)
    else
        eval "$OK_PATH"
    end
end
