set -g ok_path "~/bin/Ok.hs"

function ok
    if test (count $argv) -gt 0
        set -l ok_dir (eval $ok_path -sl)
        set -l ok_cmd (eval $ok_path -s $argv)
        pushd $ok_dir
        eval $ok_cmd
        popd
    else
        eval $ok_path -s
    end
end
