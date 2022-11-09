import argparse
import json
import re
import sys
import unicodedata

sups = str.maketrans(
    "0123456789abcdefghijklmnopqrstuvwxyz", "⁰¹²³⁴⁵⁶⁷⁸⁹ᵃᵇᶜᵈᵉᶠᵍʰⁱʲᵏˡᵐⁿᵒᵖ𐞥ʳˢᵗᵘᵛʷˣʸᶻ"
)
nfkc = lambda w: unicodedata.normalize("NFKC", w)
nfkd = lambda w: unicodedata.normalize("NFKD", w)
alphas = lambda w: "".join(filter(str.isalpha, w))
norm = lambda w: alphas(nfkd(w)).lower().replace("i", "ı")
tone = lambda w, t: re.sub("[auıoe]", lambda c: nfkc(c.group(0).replace("ı", "i") + t), w, 1)
t5 = lambda w: tone(w, "\u0302")
sup = lambda w: w.lower().translate(sups)


class ExpandSerialException(Exception):
    pass


def expand(serial, dictionary):
    def lookup_frame(w):
        if not (entry := dictionary.get(w)):
            raise ExpandSerialException(f"I don't know the word **{w}**.")
        if "verb_class" in entry:
            return ["0"]
        elif frame := entry.get("frame"):
            return frame.split()
        else:
            raise ExpandSerialException(f"I don't know the frame of **{w}**.")
    frames = [lookup_frame(w) for w in serial]

    total_frame = [w[0] for w in frames[-1]]
    for i, left in list(enumerate(frames))[-2::-1]:
        if left[-1] == "e":
            total_frame = ["c"]
            continue
        num_c = left.count("c")
        num_ja = int(left[-1][0]) if left[-1][0].isdigit() else 0
        if num_ja > len(total_frame):
            raise ExpandSerialException(
                f"**{serial[i]}** needs a {num_ja}-ary relation but **{' '.join(serial[i+1:])}** is only {len(total_frame)}-ary."
            )
        total_frame = ["c"] * num_c + total_frame[num_ja:]

    vars = [chr(65 + i) for i in range(len(total_frame))]
    steps = [[t5(" ".join(serial)), *vars]]
    pre = []
    fresh = 0
    for i, (word, frame) in enumerate(zip(serial, frames)):
        is_last = i == len(serial) - 1
        num_c = len(frame) if is_last else frame.count("c")
        vars_now = vars[:num_c]
        jas = []
        if frame[-1][0].isdigit():
            for index in frame[-1][1:]:
                if index == "x":
                    fresh += 1
                    jas.append("ja" + sup(str(fresh)))
                else:
                    v = vars_now[ord(index) - ord("i")]
                    jas.append("ja" + sup(v) if len(v) == 1 else v)
        vars[:num_c] = jas
        old_pre = pre[:]
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
                step.append(f" (+ {word} {vars[0]} pẽ {rest})")
            steps.append(step)
        if word in ["taq", "mu", "cuoı", "ıq", "jeo"]:
            pre = old_pre
            jas = [vars_now[ord(i) - ord("i")] for i in frame[-1][1:]]
            vars[: len(jas)] = jas
            step = [*pre, t5(rest), *vars]
            steps.append(step)

    return (total_frame, steps)


def expand_and_format(serial_string, dictionary):
    serial = [norm(w) for w in serial_string.split()]
    try:
        total_frame, steps = expand(serial, dictionary)
    except ExpandSerialException as e:
        return f"Error: {e}"
    return (
        f"**{' '.join(serial)}** (`{' '.join(total_frame)}`):\n```\n  "
        + "\n= ".join(map(" ".join, steps))
        + "\n```"
    )


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-d", "--dictionary", metavar="FILE", action="append", required=True, dest="dictionary_files")
    parser.add_argument("serial", nargs="+")
    args = parser.parse_args()
    dictionary = dict()
    for dictionary_file in args.dictionary_files:
        with open(dictionary_file) as f:
            dictionary |= {e["toaq"]: e for e in json.load(f)}
    print(expand_and_format(" ".join(args.serial), dictionary))
