import discord
import os, sys
import subprocess

client = discord.Client()

@client.event
async def on_ready():
    print('We have logged in as {0.user}'.format(client))

@client.event
async def on_message(message):
    if message.author == client.user:
        return

    if message.content.startswith('$tree '):
        _, sentence = message.content.split(None, 1)
        # run "stack install" to put zugai-exe on path
        run1 = subprocess.run(["zugai-exe", "--to-xbar-svg"], input=sentence.encode())
        if run1.returncode != 0:
            print(run1)
            await message.channel.send('Failed to parse.')
            return

        # put "inkscape" on path
        run2 = subprocess.run(["inkscape", "output.svg", "-h", "500", "--export-filename=output.png"])
        if run2.returncode != 0:
            print(run2)
            await message.channel.send('Failed to convert to png.')
            return

        file = discord.File("output.png", filename="image.png")
        #embed = discord.Embed()
        #embed.set_thumbnail(url="attachment://image.png")
        await message.channel.send(file=file)
    
    elif message.content.startswith('$english ') or message.content.startswith('$logic '):
        cmd, sentence = message.content.split(None, 1)
        run1 = subprocess.run(["zugai-exe", "--to-" + cmd.strip(" $")], input=sentence.encode(), capture_output=True)
        if run1.returncode != 0:
            print(run1)
            await message.channel.send('Failed to parse.')
            return
        print(run1)
        await message.channel.send(run1.stdout.decode())

client.run(os.environ['ZUGAI_DISCORD_TOKEN'])

