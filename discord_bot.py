import discord
import os, io
from zugai import RunException, run, latex_png

client = discord.Client()

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
            with latex_png(sentence.encode()) as f:
                file = discord.File(f, filename="image.png")
                await message.channel.send(file=file)
        elif cmd in ("english", "logic", "structure"):
            txt = run("parsing", ["zugai-exe", "--to-" + cmd], input=sentence.encode())
            await message.channel.send(txt.decode())
        elif cmd == "boxes":
            txt = run("parsing", ["zugai-exe", "--to-" + cmd], input=sentence.encode())
            file = discord.File(io.BytesIO(txt), filename="result.html")
            await message.channel.send(file=file)
    except RunException as e:
        await message.channel.send(str(e))

client.run(os.environ['ZUGAI_DISCORD_TOKEN'])

