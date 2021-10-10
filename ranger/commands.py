from ranger.api.commands import Command

class fzf_select(Command):
    """
    :fzf_select

    Find a file using fzf.

    With a prefix argument select only directories.

    See: https://github.com/junegunn/fzf
    """
    def execute(self):
        import subprocess
        import os.path
        if self.quantifier:
            # match only directories
            command="fd -I -t d --hidden | fzf +m --preview '$([[ -d {} ]] && echo ls || echo cat) {}'"
            # command="find -L . \( -path '*/\.*' -o -fstype 'dev' -o -fstype 'proc' \) -prune \
            # -o -type d -print 2> /dev/null | sed 1d | cut -b3- | fzf +m"
        else:
            # match files and directories
            command="fd -I --hidden | fzf +m --preview '$([[ -d {} ]] && echo ls || echo cat) {}'"
            # command="find -L . \( -path '*/\.*' -o -fstype 'dev' -o -fstype 'proc' \) -prune \
            # -o -print 2> /dev/null | sed 1d | cut -b3- | fzf +m"

        fzf = self.fm.execute_command(command, universal_newlines=True, stdout=subprocess.PIPE)
        stdout, stderr = fzf.communicate()
        if fzf.returncode == 0:
            fzf_file = os.path.abspath(stdout.rstrip('\n'))
            if os.path.isdir(fzf_file):
                self.fm.cd(fzf_file)
            else:
                self.fm.select_file(fzf_file)

class wav_to_ogg(Command):
    """
    :wav_to_ogg <file>

    Converts a wav file to ogg
    """
    def execute(self):
        # if self.arg(1):
        #     file = self.rest(1)
        # else:
        #     self.fm.notify("Usage: wav_to_ogg <file>", bad=True)
        #     return

        file = self.fm.thisfile.path
        if not file.endswith(".wav"):
            self.fm.notify("Usage: wav_to_ogg <file> ... must end in .wav", bad=True)
            return

        command = "ffmpeg -i '" + file + "' '" + file.replace(".wav", ".ogg") + "'"

        self.fm.notify(command)

        self.fm.execute_command(command)
