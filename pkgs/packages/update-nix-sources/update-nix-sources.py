import argparse
# import subprocess
from typing import BinaryIO

import tree_sitter_nix as tsnix
from github import Github, GithubException
from tree_sitter import Language, Parser, Node

NIX_LANGUAGE = Language(tsnix.language())


def parse(file: BinaryIO) -> Node:
    parser = Parser(NIX_LANGUAGE)
    tree = parser.parse(file.read())
    return tree.root_node


def get_github_nodes(root_node) -> [Node]:
    query = NIX_LANGUAGE.query("""
    ((apply_expression
      function: (variable_expression name: (identifier) @name)
      argument: (attrset_expression)) @node
     (#eq? @name "fetchFromGitHub"))
    """)

    return query.captures(root_node)["node"]


def update_github_node(node: Node):
    query = NIX_LANGUAGE.query("""
    ((apply_expression
      argument: (attrset_expression
        (binding_set
          (binding
            attrpath: (attrpath (identifier) @owner)
            expression: (string_expression (string_fragment) @owner_str))
          (binding
            attrpath: (attrpath (identifier) @repo)
            expression: (string_expression (string_fragment) @repo_str))
          (binding
            attrpath: (attrpath (identifier) @rev)
            expression: (string_expression (string_fragment) @rev_str))
          (binding
            attrpath: (attrpath (identifier) @hash)
            expression: (string_expression (string_fragment) @hash_str)))))
     (#eq? @owner "owner")
     (#eq? @repo "repo"))
    """)

    captures = query.captures(node)

    owner = captures["owner_str"][0].text.decode()
    repo = captures["repo_str"][0].text.decode()

    github = Github()
    repo = github.get_repo(f"{owner}/{repo}")

    try:
        latest_rev = repo.get_latest_release().tag_name
    except GithubException:
        commits = repo.get_commits()
        latest_rev = commits[0]

        # tags = repo.get_tags()
        # print([tag for tag in tags])
        # latest_release = "v0000"

    print(latest_rev)

   print(captures["rev_str"][0].byte_range)
    print(captures["hash_str"][0].byte_range)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("file", type=argparse.FileType("r+b"))
    args = parser.parse_args()

    root_node = parse(args.file)
    github_nodes = get_github_nodes(root_node)

    for node in github_nodes:
        update_github_node(node)


if __name__ == "__main__":
    main()
