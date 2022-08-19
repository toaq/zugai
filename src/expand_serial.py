import json
import re
import sys
import unicodedata

sups = "ᵃᵇᶜᵈᵉᶠᵍʰⁱʲᵏˡᵐⁿᵒᵖ𐞥ʳˢᵗᵘᵛʷˣʸᶻ"
nfkc = lambda w: unicodedata.normalize("NFKC", w)
nfkd = lambda w: unicodedata.normalize("NFKD", w)
alphas = lambda w: "".join(filter(str.isalpha, w))
norm = lambda w: alphas(nfkd(w)).lower().replace("i", "ı")
tone = lambda w, t: re.sub("[aeiıou]", lambda c: nfkc(c.group(0) + t), w, 1)
t5 = lambda w: tone(w, "\u0302")
sup = lambda w: w[-1] if w[-1] in sups else sups[ord(w[-1].lower()) - ord("a")]


def expand(serial, dictionary):
    frames = []
    for w in serial:
        if w not in dictionary:
            raise Exception(f"I don't know the word **{w}**.")
        entry = dictionary[w]
        if "verb_class" in entry:
            frames.append(["0"])
        elif "frame" in entry and entry["frame"]:
            frames.append([w for w in entry["frame"].split()])
        else:
            raise Exception(f"I don't know the frame of **{w}**.")

    total_frame = [w[0] for w in frames[-1]]
    for i, left in list(enumerate(frames))[-2::-1]:
        num_c = left.count("c")
        num_ja = int(left[-1][0]) if left[-1][0].isdigit() else 0
        if num_ja > len(total_frame):
            raise Exception(
                f"**{serial[i]}** serializes with more arguments than **{' '.join(serial[i+1:])}** offers."
            )
        total_frame = ["c"] * num_c + total_frame[num_ja:]

    vars = [chr(65 + i) for i in range(len(total_frame))]
    steps = [[t5(" ".join(serial)), *vars]]
    pre = []
    for i, (word, frame) in enumerate(zip(serial, frames)):
        is_last = i == len(serial) - 1
        num_c = len(frame) if is_last else frame.count("c")
        vars_now = vars[:num_c]
        coi = lambda c: "ja" if c == "x" else "ja" + sup(vars_now[ord(c) - ord("i")])
        jas = [coi(i) for i in frame[-1][1:]] if frame[-1][0].isdigit() else []
        vars[:num_c] = jas
        if "a" not in frame:
            pre.append(t5(word))
        pre += vars_now
        if not is_last:
            rest = " ".join(serial[i + 1 :])
            if frame[-1] == "e":
                steps.append([*pre, "baq " + rest])
                break
            step = [*pre, t5(rest), *vars]
            if "a" in frame:
                step.append(f" (+ {word} {vars[0]})")
            steps.append(step)

    return (total_frame, steps)


if __name__ == "__main__":
    if len(sys.argv) < 3:
        sys.exit(f"Usage: python expand_serial.py dictionary.json leo sho joe")
    with open(sys.argv[1]) as f:
        dictionary = json.load(f)
    dictionary = {e["toaq"]: e for e in dictionary}
    serial = [norm(w) for w in sys.argv[2:]]
    try:
        total_frame, steps = expand(serial, dictionary)
    except Exception as e:
        sys.exit(f"Error: {e}")
    print(f"**{' '.join(serial)}** (`{' '.join(total_frame)}`):")
    print("```")
    equals = " "
    for step in steps:
        print(equals, " ".join(step))
        equals = "="
    print("```")
