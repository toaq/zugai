import discord
import os, sys, io
import subprocess

client = discord.Client()

class RunException(Exception):
    pass

def run(verbing, cmd_args, **kwargs):
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

@client.event
async def on_ready():
    print('We have logged in as {0.user}'.format(client))

@client.event
async def on_message(message):
    if message.author == client.user:
        return

    if not message.content.startswith('%'): return
    cmd, sentence = message.content.split(None, 1)
    cmd = cmd.strip('%')

    try:
        if cmd == "svg":
            # run "stack install" to put zugai-exe on path
            svg = run("parsing", ["zugai-exe", "--to-xbar-svg"], input=sentence.encode())
            # put "inkscape" on path
            png = run("converting to png", ["inkscape",
                "--pipe", "-h", "800",
                "--export-filename=-", "--export-type=png"], input=svg)
            file = discord.File(io.BytesIO(png), filename="image.png")
            await message.channel.send(file=file)
        elif cmd == "tree":
            tex = run("parsing", ["zugai-exe", "--to-xbar-latex"], input=sentence.encode())
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
            with open("a.png", "rb") as f:
                file = discord.File(f, filename="image.png")
                await message.channel.send(file=file)
        elif cmd in ("english", "logic"):
            txt = subprocess.run("parsing", ["zugai-exe", "--to-" + cmd], input=sentence.encode())
            await message.channel.send(txt.decode())
    except RunException as e:
        await message.channel.send(str(e))

client.run(os.environ['ZUGAI_DISCORD_TOKEN'])

