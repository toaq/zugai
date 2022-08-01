# curl -X POST https://toadua.uakci.pl/api -H 'Content-Type: application/json' -d '{"action":"search","query":["term",""]}' > toadua-dump.json
# jq -c '[.results | .[] |  select(.scope == "en"  and .score >= 0 and (.head | index(" ") | not)) | {head, body}]' < toadua-dump.json > toadua-basic.json

import json
import re
import sys

def gloss(body, head):
    if (m := re.search("'([a-z .]+)';", body)):
        return m.group(1)
    body = body.split(";")[0].strip()
    body = re.sub("\.$", "", body)
    body = re.sub("\(.+\)$", "", body)
    if body.count("▯") >= 3:
        body = "▯".join(body.split("▯")[:2])+"▯"
    body = body.strip()
    body = re.sub(r" (of|for|to|by|from)? ▯$", "", body)
    if (m := re.search(r"^▯ (?:is|are) (?:(?:a|an|the) )?([^▯]+)$", body)):
        return m.group(1).split("/")[0].strip()
    if (m := re.search(r"^▯ ([^▯]+)( ▯)?$", body)):
        return m.group(1).split("/")[0].strip()
    return None

for entry in sorted(json.load(open("toadua-basic.json")), key=lambda x: x['head']):
    head = entry['head']
    body = entry['body']
    g = gloss(body, head)
    if g and 1 <= len(g) <= 16 and len(head) <= 30: print(head, g)
