#!/usr/bin/env python3
"""Prepare the curated arXiv author-year-word tensor used by the README example.

The script reads the Kaggle arXiv metadata JSONL file, finds abstracts written by
the selected statistics and machine-learning authors, samples up to a fixed
number of papers per author-year cell, and writes CSV files that can be consumed
by the TTM-HOSVD R script.
"""

import argparse
import csv
import json
import random
import re
from collections import Counter, defaultdict
from pathlib import Path


# Display names and matching names for the 21 senior authors in the arXiv example.
AUTHORS = [
    ("Arora, Sanjeev", "Sanjeev Arora"),
    ("Barber, Rina Foygel", "Rina Foygel Barber"),
    ("Belkin, Mikhail", "Mikhail Belkin"),
    ("Bengio, Yoshua", "Yoshua Bengio"),
    ("Candes, Emmanuel", "Emmanuel Candes"),
    ("Fan, Jianqing", "Jianqing Fan"),
    ("Jordan, Michael I.", "Michael I Jordan"),
    ("Jurafsky, Dan", "Dan Jurafsky"),
    ("LeCun, Yann", "Yann LeCun"),
    ("Leskovec, Jure", "Jure Leskovec"),
    ("Levina, Elizaveta", "Elizaveta Levina"),
    ("Ma, Tengyu", "Tengyu Ma"),
    ("Manning, Christopher D.", "Christopher D Manning"),
    ("Meinshausen, Nicolai", "Nicolai Meinshausen"),
    ("Montanari, Andrea", "Andrea Montanari"),
    ("Smith, Noah A.", "Noah A Smith"),
    ("Tibshirani, Ryan", "Ryan Tibshirani"),
    ("Wainwright, Martin", "Martin Wainwright"),
    ("Yu, Bin", "Bin Yu"),
    ("Zhu, Ji", "Ji Zhu"),
    ("Zou, James", "James Zou"),
]

# Reference author groups from the paper. These are used only as metadata.
CLUSTERS = {
    "cluster_statistical_estimation": {
        "Barber, Rina Foygel", "Belkin, Mikhail", "Candes, Emmanuel", "Fan, Jianqing",
        "Jordan, Michael I.", "Levina, Elizaveta", "Meinshausen, Nicolai",
        "Montanari, Andrea", "Tibshirani, Ryan", "Wainwright, Martin", "Yu, Bin", "Zhu, Ji",
    },
    "cluster_network_data": {
        "Jordan, Michael I.", "Leskovec, Jure", "Levina, Elizaveta", "Meinshausen, Nicolai",
        "Montanari, Andrea", "Yu, Bin", "Zhu, Ji", "Zou, James",
    },
    "cluster_theoretical_ml": {
        "Arora, Sanjeev", "Belkin, Mikhail", "Jordan, Michael I.", "Ma, Tengyu",
        "Montanari, Andrea", "Wainwright, Martin",
    },
    "cluster_nlp_neural_networks": {
        "Bengio, Yoshua", "Jurafsky, Dan", "LeCun, Yann", "Leskovec, Jure",
        "Manning, Christopher D.", "Smith, Noah A.",
    },
    "cluster_explainable_stats_ml_applications": {"Yu, Bin", "Zhu, Ji", "Zou, James"},
}

# Lightweight stopword list used before constructing the word-count tensor.
STOPWORDS = {
    "a", "an", "and", "are", "as", "at", "be", "been", "by", "can", "for", "from",
    "has", "have", "in", "into", "is", "it", "its", "may", "more", "of", "on", "or",
    "our", "that", "the", "their", "these", "this", "to", "using", "use", "used",
    "we", "with", "within", "without", "paper", "papers", "study", "studies", "show",
    "shows", "result", "results", "new", "also", "one", "two", "via", "based",
    "propose", "proposed", "provide", "approach", "were", "was", "which",
    "when", "such", "but", "not", "where", "under", "both", "any", "only",
    "than", "each", "how", "over", "well", "some", "all", "other", "same",
    "between", "many", "first", "then", "there", "those", "what", "will",
    "while", "through", "however",
}


def normalize_name(text):
    """Convert an author name to a simple ASCII matching key."""
    text = text.encode("ascii", "ignore").decode("ascii")
    text = re.sub(r"[^a-zA-Z]+", " ", text).lower()
    return re.sub(r"\s+", " ", text).strip()


def drop_initials(key):
    """Remove single-letter initials from a normalized author key."""
    return re.sub(r"\s+", " ", re.sub(r"\b[a-z]\b", " ", key)).strip()


def first_last(key):
    """Return a first-name/last-name key for robust author matching."""
    parts = normalize_name(key).split()
    if not parts:
        return ""
    return f"{parts[0]} {parts[-1]}"


def author_rows():
    """Build the author metadata table, including reference cluster labels."""
    rows = []
    for author, match_name in AUTHORS:
        key = normalize_name(match_name)
        row = {
            "author": author,
            "match_name": match_name,
            "key": key,
            "key_no_initials": drop_initials(key),
            "first_last_key": first_last(key),
        }
        for cluster, members in CLUSTERS.items():
            row[cluster] = str(author in members).upper()
        rows.append(row)
    return rows


def paper_author_keys(authors_parsed):
    """Create comparable author-name keys from an arXiv metadata record."""
    keys = set()
    for author in authors_parsed or []:
        last = author[0] if len(author) > 0 else ""
        first = author[1] if len(author) > 1 else ""
        suffix = author[2] if len(author) > 2 else ""
        key = normalize_name(f"{first} {suffix} {last}")
        if key:
            keys.add(key)
            keys.add(drop_initials(key))
            keys.add(first_last(key))
    return keys


def extract_year(paper):
    """Extract the publication year from arXiv version metadata."""
    versions = paper.get("versions") or []
    if versions:
        created = versions[0].get("created", "")
        match = re.search(r"[12][0-9]{3}", created)
        if match:
            return int(match.group(0))
    update_date = paper.get("update_date", "")
    if len(update_date) >= 4 and update_date[:4].isdigit():
        return int(update_date[:4])
    return None


def tokenize(text):
    """Tokenize an abstract into filtered lowercase word tokens."""
    text = text.encode("ascii", "ignore").decode("ascii").lower()
    text = re.sub(r"https?://\S+", " ", text)
    text = re.sub(r"[^a-z ]+", " ", text)
    tokens = re.split(r"\s+", text)
    return [tok for tok in tokens if 3 <= len(tok) <= 25 and tok not in STOPWORDS]


def collect_papers(metadata, authors, start_year, end_year):
    """Scan the arXiv metadata file and keep papers matching selected authors."""
    lookup = defaultdict(list)
    for row in authors:
        lookup[row["key"]].append(row["author"])
        lookup[row["key_no_initials"]].append(row["author"])
        lookup[row["first_last_key"]].append(row["author"])

    matched = []
    with open(metadata, "r", encoding="utf-8") as handle:
        for i, line in enumerate(handle, start=1):
            if i % 100000 == 0:
                print(f"Scanned {i:,} records; matched {len(matched):,} author-paper rows", flush=True)
            paper = json.loads(line)
            year = extract_year(paper)
            if year is None or year < start_year or year > end_year:
                continue
            keys = paper_author_keys(paper.get("authors_parsed"))
            selected_authors = sorted({author for key in keys for author in lookup.get(key, [])})
            if not selected_authors:
                continue
            for author in selected_authors:
                matched.append({
                    "selected_author": author,
                    "id": paper.get("id", ""),
                    "year": year,
                    "title": re.sub(r"\s+", " ", paper.get("title", "")).strip(),
                    "abstract": re.sub(r"\s+", " ", paper.get("abstract", "")).strip(),
                    "categories": paper.get("categories", ""),
                })
    return matched


def sample_papers(matched, authors, years, per_year, seed):
    """Sample up to `per_year` papers for each author and target year."""
    rng = random.Random(seed)
    by_author = defaultdict(list)
    for row in matched:
        by_author[row["selected_author"]].append(row)

    used = set()
    selected = []
    for author in [row["author"] for row in authors]:
        papers = by_author.get(author, [])
        for target_year in years:
            exact = [row for row in papers if row["year"] == target_year and row["id"] not in used]
            rng.shuffle(exact)
            chosen = exact[:per_year]
            if len(chosen) < per_year:
                window = [
                    row for row in papers
                    if abs(row["year"] - target_year) <= 2
                    and row["year"] != target_year
                    and row["id"] not in used
                    and row["id"] not in {item["id"] for item in chosen}
                ]
                rng.shuffle(window)
                chosen.extend(window[:per_year - len(chosen)])
            for row in chosen:
                row = dict(row)
                row["target_year"] = target_year
                row["sample_window"] = "same_year" if row["year"] == target_year else "within_2_years"
                selected.append(row)
                used.add(row["id"])
    return selected


def write_csv(path, rows, fieldnames=None):
    """Write a list of dictionaries to CSV, creating parent directories."""
    path.parent.mkdir(parents=True, exist_ok=True)
    if fieldnames is None:
        fieldnames = list(rows[0].keys()) if rows else []
    with open(path, "w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def build_tensor(selected, authors, years, max_vocab, min_total_count):
    """Construct the token-by-author-year count matrix and related metadata."""
    cell_tokens = defaultdict(list)
    global_counts = Counter()
    for row in selected:
        cell = f"{row['selected_author']}__{row['target_year']}"
        tokens = tokenize(row["abstract"])
        cell_tokens[cell].extend(tokens)
        global_counts.update(tokens)

    vocab = [word for word, count in global_counts.most_common() if count >= min_total_count][:max_vocab]
    vocab_set = set(vocab)
    cells = [(author["author"], year, f"{author['author']}__{year}") for author in authors for year in years]
    matrix = []
    empty_cells = []
    for author, year, cell in cells:
        counts = Counter(tok for tok in cell_tokens[cell] if tok in vocab_set)
        if not counts:
            empty_cells.append({"author": author, "year": year, "cell": cell})
            matrix.append([1 for _ in vocab])
        else:
            matrix.append([counts[word] for word in vocab])
    return vocab, cells, matrix, empty_cells, global_counts


def main():
    """Parse command-line arguments and write all prepared arXiv CSV outputs."""
    parser = argparse.ArgumentParser()
    parser.add_argument("--metadata", required=True)
    parser.add_argument("--output-dir", required=True)
    parser.add_argument("--start-year", type=int, default=2005)
    parser.add_argument("--end-year", type=int, default=2024)
    parser.add_argument("--per-year", type=int, default=3)
    parser.add_argument("--max-vocab", type=int, default=5000)
    parser.add_argument("--min-total-count", type=int, default=5)
    parser.add_argument("--seed", type=int, default=1234)
    args = parser.parse_args()

    output_dir = Path(args.output_dir)
    authors = author_rows()
    years = list(range(args.start_year, args.end_year + 1))

    matched = collect_papers(args.metadata, authors, args.start_year, args.end_year)
    selected = sample_papers(matched, authors, years, args.per_year, args.seed)
    vocab, cells, matrix, empty_cells, global_counts = build_tensor(
        selected, authors, years, args.max_vocab, args.min_total_count
    )

    write_csv(output_dir / "arxiv_authors.csv", authors)
    write_csv(output_dir / "matched_author_papers.csv", matched)
    write_csv(output_dir / "selected_author_year_papers.csv", selected)
    write_csv(output_dir / "tensor_cells.csv", [
        {"author": author, "year": year, "cell": cell} for author, year, cell in cells
    ])
    write_csv(output_dir / "empty_cells.csv", empty_cells, fieldnames=["author", "year", "cell"])
    write_csv(output_dir / "vocabulary.csv", [
        {"token": word, "count": global_counts[word]} for word in vocab
    ])

    with open(output_dir / "Y3_author_year_word_counts.csv", "w", newline="", encoding="utf-8") as handle:
        writer = csv.writer(handle)
        writer.writerow(["token"] + [cell for _, _, cell in cells])
        for word_index, word in enumerate(vocab):
            writer.writerow([word] + [matrix[cell_index][word_index] for cell_index in range(len(cells))])

    print(f"Matched {len(matched):,} author-paper rows")
    print(f"Selected {len(selected):,} author-year paper rows across {len({row['id'] for row in selected}):,} unique papers")
    print(f"Tensor dimensions: authors={len(authors)} years={len(years)} vocabulary={len(vocab)}")
    print(f"Output: {output_dir}")


if __name__ == "__main__":
    main()
