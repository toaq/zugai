from contextlib import contextmanager
from typing import List
import subprocess

class RunException(Exception):
    """
    Something went wrong while invoking zugai.
    """
    pass

def run(verbing: str, cmd_args: List[str], **kwargs):
    """
    Run a command (`cmd_args`) described in error messages by the gerund `verbing`.

    Times out after 7 seconds.
    """
    try:
        result = subprocess.run(cmd_args, **kwargs, check=True, timeout=7, capture_output=True)
        return result.stdout
    except subprocess.CalledProcessError as e:
        if e.stderr:
            raise RunException(f"Error while {verbing}:\n```\n{e.stderr.decode().strip()}\n```")
        else:
            raise RunException(f"Error while {verbing}.")
    except subprocess.TimeoutExpired as e:
        raise RunException(f"Timed out while {verbing}.")

@contextmanager
def latex_png(sentence: str):
    """
    Convert the given sentence to LaTeX, then to PDF, then to PNG, then present
    the PNG file in a `with` context.

    Example:
    ```py
    with latex_png("Hỉo ka") as png_file:
        print(len(png_file.read()))
    ```
    """
    tex = run("parsing", ["zugai-exe", "--to-xbar-latex"],
              input=bytes(sentence, encoding="utf-8"))
    with open("a.tex", "wb") as f: f.write(tex)
    run("converting to pdf", ["xelatex", "a.tex"])
    run("converting to png", ["convert",
        "-define", "png:color-type=6",
        "-density", "500", "-quality", "100",
        "-background", "#36393E",
        "-alpha", "remove", "-alpha", "off",
        "-trim",
        "-resize", "x1000>",
        "-bordercolor", "#36393E", "-border", "40x20",
        "a.pdf", "a.png"])
    f = open("a.png", "rb")
    try:
        yield f
    finally:
        f.close()
