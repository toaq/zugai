import discord
import os, sys, io
import subprocess

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

    if cmd == "tree":
        # run "stack install" to put zugai-exe on path
        run1 = subprocess.run(["zugai-exe", "--to-xbar-svg"], input=sentence.encode(), capture_output=True)
        if run1.returncode != 0:
            print(run1)
            await message.channel.send(f'Failed to parse.\n```\n{run1.stderr.decode().strip()}\n```')
            return

        # put "inkscape" on path
        run2 = subprocess.run(["inkscape", "--pipe", "-h", "800", "--export-filename=-", "--export-type=png"], input=run1.stdout, capture_output=True)
        if run2.returncode != 0:
            print(run2)
            await message.channel.send('Failed to convert to png.')
            return

        file = discord.File(io.BytesIO(run2.stdout), filename="image.png")
        #embed = discord.Embed()
        #embed.set_thumbnail(url="attachment://image.png")
        await message.channel.send(file=file)
    elif cmd in ("english", "logic"):
        run1 = subprocess.run(["zugai-exe", "--to-" + cmd], input=sentence.encode(), capture_output=True)
        if run1.returncode != 0:
            print(run1)
            await message.channel.send(f'Failed to parse.\n```\n{run1.stderr.decode().strip()}\n```')
            return
        print(run1)
        await message.channel.send(run1.stdout.decode())

client.run(os.environ['ZUGAI_DISCORD_TOKEN'])

