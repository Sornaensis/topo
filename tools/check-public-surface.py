#!/usr/bin/env python3
"""Validate the public surface inventory against Stack package metadata.

The inventory intentionally lists every currently exposed library module once.
This check fails when a package exposes a new module without adding an inventory
mapping, when an inventoried module is no longer exposed, or when a feature group
lacks ownership/status/surface/test metadata.
"""

from __future__ import annotations

import json
import re
import sys
from pathlib import Path
from typing import Any


ROOT = Path(__file__).resolve().parents[1]
INVENTORY_PATH = ROOT / "docs" / "inventory" / "public-surface.json"
VALID_STATUSES = {"present", "partial", "planned", "compatibility", "internal", "waived"}
VALID_FEATURE_AVAILABILITY = {"present", "missing"}


def strip_inline_comment(line: str) -> str:
    """Remove YAML comments while preserving # characters inside quotes."""

    in_single = False
    in_double = False
    escaped = False

    for index, char in enumerate(line):
        if char == "\\" and in_double and not escaped:
            escaped = True
            continue
        if char == "'" and not in_double:
            in_single = not in_single
        elif char == '"' and not in_single and not escaped:
            in_double = not in_double
        elif char == "#" and not in_single and not in_double:
            return line[:index].rstrip()
        escaped = False

    return line.rstrip()


def clean_yaml_line(line: str) -> tuple[int, str]:
    uncommented = strip_inline_comment(line)
    return len(uncommented) - len(uncommented.lstrip(" ")), uncommented.strip()


def unquote(value: str) -> str:
    value = value.strip()
    if len(value) >= 2 and value[0] == value[-1] and value[0] in {'"', "'"}:
        return value[1:-1]
    return value


def split_top_level_commas(value: str) -> list[str]:
    items: list[str] = []
    item: list[str] = []
    in_single = False
    in_double = False
    escaped = False
    depth = 0

    for char in value:
        if char == "\\" and in_double and not escaped:
            escaped = True
            item.append(char)
            continue
        if char == "'" and not in_double:
            in_single = not in_single
        elif char == '"' and not in_single and not escaped:
            in_double = not in_double
        elif not in_single and not in_double:
            if char in "[{(":
                depth += 1
            elif char in "]})" and depth > 0:
                depth -= 1
        if char == "," and depth == 0 and not in_single and not in_double:
            parsed = "".join(item).strip()
            if parsed:
                items.append(parsed)
            item = []
        else:
            item.append(char)
        escaped = False

    parsed = "".join(item).strip()
    if parsed:
        items.append(parsed)
    return items


def split_top_level_key_value(value: str) -> tuple[str, str] | None:
    in_single = False
    in_double = False
    escaped = False
    depth = 0

    for index, char in enumerate(value):
        if char == "\\" and in_double and not escaped:
            escaped = True
            continue
        if char == "'" and not in_double:
            in_single = not in_single
        elif char == '"' and not in_single and not escaped:
            in_double = not in_double
        elif not in_single and not in_double:
            if char in "[{(":
                depth += 1
            elif char in "]})" and depth > 0:
                depth -= 1
            elif char == ":" and depth == 0:
                return value[:index].strip(), value[index + 1 :].strip()
        escaped = False

    return None


def parse_inline_map(value: str) -> dict[str, str]:
    value = value.strip()
    if not (value.startswith("{") and value.endswith("}")):
        return {}

    mapping: dict[str, str] = {}
    for entry in split_top_level_commas(value[1:-1]):
        key_value = split_top_level_key_value(entry)
        if key_value is None:
            continue
        key, item_value = key_value
        mapping[unquote(key)] = unquote(item_value)
    return mapping


def parse_location_value(value: str) -> str | None:
    value = value.strip()
    if not value:
        return None
    if value.startswith("{") and value.endswith("}"):
        path = parse_inline_map(value).get("path")
        return parse_location_value(path) if path is not None else None
    return unquote(value)


def parse_stack_package_item(item: str) -> str | None:
    item = item.strip()
    if not item:
        return None
    if item.startswith("{") and item.endswith("}"):
        location = parse_inline_map(item).get("location")
        return parse_location_value(location) if location is not None else None
    if item.startswith("location:"):
        return parse_location_value(item.split(":", 1)[1].strip())
    return unquote(item)


def split_inline_list(value: str) -> list[str]:
    """Split a small YAML inline list without depending on PyYAML."""

    value = value.strip()
    if value == "[]":
        return []
    if not (value.startswith("[") and value.endswith("]")):
        return [unquote(value)] if value else []

    return [unquote(item) for item in split_top_level_commas(value[1:-1])]


def parse_scalar_or_inline_list(value: str) -> list[str]:
    value = strip_inline_comment(value).strip()
    if not value:
        return []
    return split_inline_list(value)


def load_inventory() -> dict[str, Any]:
    try:
        return json.loads(INVENTORY_PATH.read_text(encoding="utf-8"))
    except FileNotFoundError:
        fail(f"Inventory not found: {INVENTORY_PATH.relative_to(ROOT)}")
    except json.JSONDecodeError as exc:
        fail(f"Inventory JSON is invalid: {exc}")


def parse_stack_packages() -> list[str]:
    stack_yaml = ROOT / "stack.yaml"
    packages: list[str] = []
    in_packages = False
    package_indent = 0
    pending_location_indent: int | None = None

    for line in stack_yaml.read_text(encoding="utf-8").splitlines():
        indent, stripped = clean_yaml_line(line)
        if not stripped or stripped.startswith("#"):
            continue

        if indent == 0 and stripped.startswith("packages:"):
            package_indent = indent
            inline_value = stripped.split(":", 1)[1].strip()
            for item in parse_scalar_or_inline_list(inline_value):
                package_path = parse_stack_package_item(item)
                if package_path:
                    packages.append(package_path)
            in_packages = not inline_value
            continue

        if indent == 0:
            in_packages = False
            pending_location_indent = None

        if not in_packages:
            continue

        if indent <= package_indent:
            in_packages = False
            pending_location_indent = None
            continue

        if pending_location_indent is not None:
            if indent <= pending_location_indent:
                pending_location_indent = None
            elif stripped.startswith("path:"):
                package_path = unquote(stripped.split(":", 1)[1].strip())
                if package_path:
                    packages.append(package_path)
                pending_location_indent = None
                continue

        if stripped.startswith("- "):
            item = stripped[2:].strip()
            if item == "location:":
                pending_location_indent = indent
                continue
            package_path = parse_stack_package_item(item)
            if package_path:
                packages.append(package_path)
        elif stripped.startswith("location:"):
            package_path = parse_location_value(stripped.split(":", 1)[1].strip())
            if package_path:
                packages.append(package_path)

    return packages


def parse_package_name(package_yaml: Path) -> str | None:
    for line in package_yaml.read_text(encoding="utf-8").splitlines():
        _indent, stripped = clean_yaml_line(line)
        if stripped.startswith("name:"):
            values = parse_scalar_or_inline_list(stripped.split(":", 1)[1])
            return values[0] if values else None
    return None


def parse_values_in_top_level_section(package_yaml: Path, section: str, field: str) -> list[str]:
    """Parse common Hpack field forms inside a top-level section.

    Supports the block-list and inline-list shapes used by Hpack files, e.g.:
    `exposed-modules:`, `exposed-modules: [A, B]`, and `source-dirs: test`.
    It is intentionally dependency-free so the drift check can run in lightweight
    CI jobs without bootstrapping a Haskell or Python package environment.
    """

    values: list[str] = []
    in_section = False
    in_field = False
    field_indent = 0

    for line in package_yaml.read_text(encoding="utf-8").splitlines():
        indent, stripped = clean_yaml_line(line)
        if not stripped or stripped.startswith("#"):
            continue

        if indent == 0:
            in_section = stripped == f"{section}:"
            in_field = False
            continue

        if not in_section:
            continue

        if stripped.startswith(f"{field}:"):
            inline_value = stripped.split(":", 1)[1].strip()
            parsed = parse_scalar_or_inline_list(inline_value)
            values.extend(parsed)
            in_field = not inline_value
            field_indent = indent
            continue

        if in_field:
            if indent <= field_indent and not stripped.startswith("- "):
                in_field = False
                continue
            if stripped.startswith("- "):
                values.extend(parse_scalar_or_inline_list(stripped[2:].strip()))

    return values


def parse_exposed_modules(package_yaml: Path) -> list[str]:
    return parse_values_in_top_level_section(package_yaml, "library", "exposed-modules")


def parse_test_source_dirs(package_yaml: Path) -> list[str]:
    source_dirs = parse_values_in_top_level_section(package_yaml, "tests", "source-dirs")
    return sorted(set(source_dirs))


def parse_test_main_files(package_yaml: Path) -> list[str]:
    main_files = parse_values_in_top_level_section(package_yaml, "tests", "main")
    return sorted(set(main_files))


def parse_haskell_imports(source_file: Path) -> set[str]:
    imports: set[str] = set()
    for line in source_file.read_text(encoding="utf-8").splitlines():
        stripped = strip_inline_comment(line).strip()
        if not stripped.startswith("import "):
            continue
        tokens = stripped.split()
        if len(tokens) >= 3 and tokens[1] == "qualified":
            imports.add(tokens[2])
        elif len(tokens) >= 2:
            imports.add(tokens[1])
    return imports


def parse_test_suite_imports(package_yaml: Path) -> set[str]:
    source_dirs = parse_test_source_dirs(package_yaml)
    main_files = parse_test_main_files(package_yaml)
    imports: set[str] = set()

    for source_dir in source_dirs:
        for main_file in main_files:
            main_path = package_yaml.parent / source_dir / main_file
            if main_path.exists():
                imports.update(parse_haskell_imports(main_path))

    return imports


def find_test_main_paths(package_yaml: Path) -> tuple[list[Path], list[str]]:
    source_dirs = parse_test_source_dirs(package_yaml)
    main_files = parse_test_main_files(package_yaml)
    found: list[Path] = []
    searched: list[str] = []

    for source_dir in source_dirs:
        for main_file in main_files:
            path = package_yaml.parent / source_dir / main_file
            searched.append(str(Path(source_dir) / main_file))
            if path.exists():
                found.append(path)

    return found, searched


def validate_test_reference(
    test_ref: str,
    group_id: str,
    inventory_packages: dict[str, Path],
    errors: list[str],
) -> None:
    parts = test_ref.split(":", 2)
    if len(parts) != 3 or parts[1] != "test" or not parts[2].strip():
        errors.append(
            f"matrix entry {group_id} has invalid test reference {test_ref!r}; "
            "expected '<package>:test:<Module.Name>'"
        )
        return

    package_name, _kind, module_name = parts
    package_yaml = inventory_packages.get(package_name)
    if package_yaml is None:
        errors.append(f"matrix entry {group_id} references tests for unknown package {package_name}")
        return
    if not package_yaml.exists():
        return

    source_dirs = parse_test_source_dirs(package_yaml)
    if not source_dirs:
        errors.append(f"matrix entry {group_id} references tests for package {package_name}, but it has no test source-dirs")
        return

    module_path = Path(*module_name.split(".")).with_suffix(".hs")
    candidate_module_paths = [package_yaml.parent / source_dir / module_path for source_dir in source_dirs]
    if not any(path.exists() for path in candidate_module_paths):
        searched = ", ".join(str(Path(source_dir) / module_path) for source_dir in source_dirs)
        errors.append(f"matrix entry {group_id} test reference {test_ref!r} does not exist; searched {searched}")
        return

    main_paths, searched_mains = find_test_main_paths(package_yaml)
    if not main_paths:
        searched = ", ".join(searched_mains) if searched_mains else "<none>"
        errors.append(f"matrix entry {group_id} test reference {test_ref!r} has no existing test suite main file; searched {searched}")
        return

    if any(path in main_paths for path in candidate_module_paths):
        return

    suite_imports = parse_test_suite_imports(package_yaml)
    if suite_imports and module_name not in suite_imports:
        errors.append(f"matrix entry {group_id} test reference {test_ref!r} is not imported by the test suite main module")


def parse_public_http_routes() -> set[tuple[str, str, str]]:
    """Parse the literal public route table in Seer.HTTP.Server.

    The runtime route table remains the authoritative source; this lightweight
    parser lets the drift check run in Python-only CI before Stack is built.
    The Haskell FeatureMatrix spec also compares against publicHttpRouteSpecs.
    """

    server_path = ROOT / "topo-seer" / "src" / "Seer" / "HTTP" / "Server.hs"
    source = server_path.read_text(encoding="utf-8")
    start = source.find("friendlyHttpRouteSpecs = map annotateHttpRouteSpec")
    end = source.find("\nspecial ::", start)
    if start == -1 or end == -1:
        fail("could not find friendlyHttpRouteSpecs block in topo-seer/src/Seer/HTTP/Server.hs")

    block = source[start:end]
    pattern = re.compile(
        r"\b(?:specialWithQuery|special|serviceWithQuery|service)\s+"
        r'"([^"]+)"\s+\[([^\]]*)\]\s+"([^"]+)"',
        re.MULTILINE | re.DOTALL,
    )
    routes: set[tuple[str, str, str]] = set()
    for method, raw_segments, operation_id in pattern.findall(block):
        segments = re.findall(r'"([^"]+)"', raw_segments)
        if not segments:
            continue
        routes.add((operation_id, method, "/" + "/".join(segments)))
    return routes


def parse_service_operations() -> set[tuple[str, str]]:
    operations: set[tuple[str, str]] = set()
    service_dir = ROOT / "topo-seer" / "src" / "Seer" / "Service"
    pattern = re.compile(r'operationSpec\s+"([^"]+)"\s+"([^"]+)"\s+"([^"]+)"')
    for service_file in service_dir.glob("*.hs"):
        if service_file.name == "Types.hs":
            continue
        source = service_file.read_text(encoding="utf-8")
        for operation_name, method_name, _description in pattern.findall(source):
            operations.add((operation_name, method_name))
    return operations


def has_tests_or_waiver(entry: dict[str, Any]) -> bool:
    tests = entry.get("tests")
    waiver = entry.get("waiver")
    has_tests = isinstance(tests, list) and all(isinstance(test, str) and test.strip() for test in tests) and bool(tests)
    has_waiver = isinstance(waiver, str) and bool(waiver.strip())
    return has_tests or has_waiver


def validate_test_refs(
    owner_label: str,
    tests: Any,
    inventory_packages: dict[str, Path],
    errors: list[str],
) -> None:
    if tests is not None and not isinstance(tests, list):
        errors.append(f"{owner_label} tests must be a list when present")
        return
    if isinstance(tests, list):
        for test_ref in tests:
            if not isinstance(test_ref, str) or not test_ref.strip():
                errors.append(f"{owner_label} has invalid test reference entry {test_ref!r}")
                continue
            validate_test_reference(test_ref, owner_label, inventory_packages, errors)


def validate_features(
    inventory: dict[str, Any],
    feature_group_ids: set[str],
    inventory_packages: dict[str, Path],
    errors: list[str],
) -> None:
    features = inventory.get("features")
    if not isinstance(features, list) or not features:
        errors.append("inventory must contain a non-empty features list")
        return

    feature_ids: set[str] = set()
    for feature in features:
        if not isinstance(feature, dict):
            errors.append("features entries must be objects")
            continue
        feature_id = feature.get("id") if isinstance(feature.get("id"), str) and feature.get("id", "").strip() else "<unknown>"
        if feature_id in feature_ids:
            errors.append(f"duplicate feature id {feature_id}")
        feature_ids.add(feature_id)

        for field in ("id", "title", "availability", "owner", "status", "featureGroup"):
            value = feature.get(field)
            if not isinstance(value, str) or not value.strip():
                errors.append(f"feature {feature_id} must set non-empty {field}")
        availability = feature.get("availability")
        if isinstance(availability, str) and availability not in VALID_FEATURE_AVAILABILITY:
            errors.append(
                f"feature {feature_id} has invalid availability {availability!r}; "
                f"expected one of {sorted(VALID_FEATURE_AVAILABILITY)}"
            )
        status = feature.get("status")
        if isinstance(status, str) and status not in VALID_STATUSES:
            errors.append(
                f"feature {feature_id} has invalid status {status!r}; "
                f"expected one of {sorted(VALID_STATUSES)}"
            )
        group_id = feature.get("featureGroup")
        if isinstance(group_id, str) and group_id not in feature_group_ids:
            errors.append(f"feature {feature_id} references unknown feature group {group_id}")
        if not has_tests_or_waiver(feature):
            errors.append(f"feature {feature_id} needs tests or an explicit waiver")
        validate_test_refs(f"feature {feature_id}", feature.get("tests"), inventory_packages, errors)


def validate_public_http_routes(
    inventory: dict[str, Any],
    feature_group_ids: set[str],
    errors: list[str],
) -> None:
    sections = inventory.get("publicHttpRoutes")
    if not isinstance(sections, list) or not sections:
        errors.append("inventory must contain a non-empty publicHttpRoutes list")
        return

    matrix_routes: set[tuple[str, str, str]] = set()
    for section in sections:
        if not isinstance(section, dict):
            errors.append("publicHttpRoutes entries must be objects")
            continue
        group_id = section.get("featureGroup")
        if not isinstance(group_id, str) or not group_id.strip():
            errors.append("publicHttpRoutes entry must set non-empty featureGroup")
        elif group_id not in feature_group_ids:
            errors.append(f"publicHttpRoutes entry references unknown feature group {group_id}")
        routes = section.get("routes")
        if not isinstance(routes, list) or not routes:
            errors.append(f"publicHttpRoutes entry {group_id!r} must contain a non-empty routes list")
            continue
        for route in routes:
            if not isinstance(route, dict):
                errors.append(f"publicHttpRoutes entry {group_id!r} contains a non-object route")
                continue
            operation_id = route.get("operationId")
            method = route.get("method")
            path = route.get("path")
            if not all(isinstance(value, str) and value.strip() for value in (operation_id, method, path)):
                errors.append(f"publicHttpRoutes entry {group_id!r} contains an invalid route {route!r}")
                continue
            key = (operation_id, method, path)
            if key in matrix_routes:
                errors.append(f"duplicate public HTTP route matrix entry {method} {path} ({operation_id})")
            matrix_routes.add(key)

    source_routes = parse_public_http_routes()
    missing = sorted(source_routes - matrix_routes)
    stale = sorted(matrix_routes - source_routes)
    if missing:
        errors.append(
            "public HTTP routes missing from feature matrix: "
            + ", ".join(f"{method} {path} ({op})" for op, method, path in missing)
        )
    if stale:
        errors.append(
            "feature matrix contains stale public HTTP routes: "
            + ", ".join(f"{method} {path} ({op})" for op, method, path in stale)
        )


def validate_service_operations(
    inventory: dict[str, Any],
    feature_group_ids: set[str],
    errors: list[str],
) -> None:
    sections = inventory.get("serviceOperations")
    if not isinstance(sections, list) or not sections:
        errors.append("inventory must contain a non-empty serviceOperations list")
        return

    matrix_operations: set[tuple[str, str]] = set()
    for section in sections:
        if not isinstance(section, dict):
            errors.append("serviceOperations entries must be objects")
            continue
        group_id = section.get("featureGroup")
        if not isinstance(group_id, str) or not group_id.strip():
            errors.append("serviceOperations entry must set non-empty featureGroup")
        elif group_id not in feature_group_ids:
            errors.append(f"serviceOperations entry references unknown feature group {group_id}")
        operations = section.get("operations")
        if not isinstance(operations, list) or not operations:
            errors.append(f"serviceOperations entry {group_id!r} must contain a non-empty operations list")
            continue
        for operation in operations:
            if not isinstance(operation, dict):
                errors.append(f"serviceOperations entry {group_id!r} contains a non-object operation")
                continue
            name = operation.get("name")
            method = operation.get("method")
            if not all(isinstance(value, str) and value.strip() for value in (name, method)):
                errors.append(f"serviceOperations entry {group_id!r} contains an invalid operation {operation!r}")
                continue
            key = (name, method)
            if key in matrix_operations:
                errors.append(f"duplicate service operation matrix entry {name} ({method})")
            matrix_operations.add(key)

    source_operations = parse_service_operations()
    missing = sorted(source_operations - matrix_operations)
    stale = sorted(matrix_operations - source_operations)
    if missing:
        errors.append(
            "service operations missing from feature matrix: "
            + ", ".join(f"{name} ({method})" for name, method in missing)
        )
    if stale:
        errors.append(
            "feature matrix contains stale service operations: "
            + ", ".join(f"{name} ({method})" for name, method in stale)
        )


def run_parser_self_tests(errors: list[str]) -> None:
    cases = {
        "topo": "topo",
        "location: topo": "topo",
        "location: { path: topo }": "topo",
        "location: { path: \"topo-seer\" }": "topo-seer",
        "{ location: { path: topo-plugin-sdk } }": "topo-plugin-sdk",
    }
    for input_value, expected in cases.items():
        actual = parse_stack_package_item(input_value)
        if actual != expected:
            errors.append(f"parser self-test failed for {input_value!r}: expected {expected!r}, got {actual!r}")

    inline_list = parse_scalar_or_inline_list('["Topo", Topo.Types, { path: topo }]')
    if inline_list != ["Topo", "Topo.Types", "{ path: topo }"]:
        errors.append(f"parser self-test failed for inline list parsing: got {inline_list!r}")

    inline_packages = [
        parsed for item in parse_scalar_or_inline_list("[{ location: { path: topo } }]")
        if (parsed := parse_stack_package_item(item))
    ]
    if inline_packages != ["topo"]:
        errors.append(f"parser self-test failed for inline packages parsing: got {inline_packages!r}")


def require_string(group: dict[str, Any], field: str, errors: list[str]) -> None:
    value = group.get(field)
    if not isinstance(value, str) or not value.strip():
        errors.append(f"feature group {group.get('id', '<unknown>')} must set non-empty {field}")


def fail(message: str) -> None:
    print(f"public-surface inventory check failed: {message}", file=sys.stderr)
    raise SystemExit(1)


def main() -> int:
    inventory = load_inventory()
    errors: list[str] = []
    run_parser_self_tests(errors)

    package_entries = inventory.get("packages")
    if not isinstance(package_entries, list) or not package_entries:
        fail("inventory must contain a non-empty packages list")

    inventory_packages: dict[str, Path] = {}
    inventory_package_paths: dict[str, str] = {}
    for entry in package_entries:
        if not isinstance(entry, dict):
            errors.append("packages entries must be objects")
            continue
        name = entry.get("name")
        package_yaml = entry.get("packageYaml")
        role = entry.get("role")
        if not isinstance(name, str) or not isinstance(package_yaml, str):
            errors.append(f"invalid package entry: {entry!r}")
            continue
        if not isinstance(role, str) or not role.strip():
            errors.append(f"package entry {name} must set non-empty role metadata")
        if name in inventory_packages:
            errors.append(f"duplicate package entry for {name}")
        path = ROOT / package_yaml
        path_key = str(path.resolve(strict=False))
        previous_package = inventory_package_paths.get(path_key)
        if previous_package is not None:
            errors.append(f"packages {previous_package} and {name} both use packageYaml {package_yaml}")
        inventory_package_paths[path_key] = name
        if not path.exists():
            errors.append(f"package {name} packageYaml does not exist: {package_yaml}")
        inventory_packages[name] = path

    # Every Stack package with an exposed library surface should have an inventory entry.
    for package_dir in parse_stack_packages():
        package_yaml = ROOT / package_dir / "package.yaml"
        if not package_yaml.exists():
            continue
        exposed = parse_exposed_modules(package_yaml)
        if not exposed:
            continue
        package_name = parse_package_name(package_yaml) or package_dir
        if package_name not in inventory_packages:
            errors.append(
                f"stack package {package_name} exposes library modules but is missing from inventory packages"
            )

    groups = inventory.get("featureGroups")
    if not isinstance(groups, list) or not groups:
        fail("inventory must contain a non-empty featureGroups list")

    feature_group_ids: set[str] = set()
    module_to_group: dict[tuple[str, str], str] = {}
    for group in groups:
        if not isinstance(group, dict):
            errors.append("featureGroups entries must be objects")
            continue

        raw_group_id = group.get("id")
        group_id = raw_group_id if isinstance(raw_group_id, str) and raw_group_id.strip() else "<unknown>"
        if group_id in feature_group_ids:
            errors.append(f"duplicate feature group id {group_id}")
        feature_group_ids.add(group_id)

        for field in ("id", "title", "owner", "status", "uiSurface", "serviceSurface", "httpSurface"):
            require_string(group, field, errors)

        status = group.get("status")
        if isinstance(status, str) and status not in VALID_STATUSES:
            errors.append(
                f"feature group {group.get('id', '<unknown>')} has invalid status {status!r}; "
                f"expected one of {sorted(VALID_STATUSES)}"
            )

        tests = group.get("tests")
        if not has_tests_or_waiver(group):
            errors.append(f"feature group {group_id} needs tests or an explicit waiver")
        validate_test_refs(f"feature group {group_id}", tests, inventory_packages, errors)

        modules_by_package = group.get("modules")
        if not isinstance(modules_by_package, dict) or not modules_by_package:
            errors.append(f"feature group {group_id} must map at least one module")
            continue

        for package_name, modules in modules_by_package.items():
            if package_name not in inventory_packages:
                errors.append(
                    f"feature group {group_id} references unknown package {package_name}"
                )
                continue
            if not isinstance(modules, list) or not modules:
                errors.append(
                    f"feature group {group_id} package {package_name} must list modules"
                )
                continue
            for module_name in modules:
                if not isinstance(module_name, str) or not module_name.strip():
                    errors.append(
                        f"feature group {group_id} package {package_name} has invalid module entry"
                    )
                    continue
                key = (package_name, module_name)
                previous = module_to_group.get(key)
                if previous is not None:
                    errors.append(
                        f"module {package_name}:{module_name} is mapped by both {previous} and {group_id}"
                    )
                module_to_group[key] = group_id

    validate_features(inventory, feature_group_ids, inventory_packages, errors)
    validate_public_http_routes(inventory, feature_group_ids, errors)
    validate_service_operations(inventory, feature_group_ids, errors)

    for package_name, package_yaml in inventory_packages.items():
        if not package_yaml.exists():
            continue
        exposed = set(parse_exposed_modules(package_yaml))
        inventoried = {module for (pkg, module), _group in module_to_group.items() if pkg == package_name}

        missing = sorted(exposed - inventoried)
        stale = sorted(inventoried - exposed)
        if missing:
            errors.append(
                f"package {package_name} has exposed modules missing from inventory: {', '.join(missing)}"
            )
        if stale:
            errors.append(
                f"package {package_name} inventory contains modules no longer exposed: {', '.join(stale)}"
            )

    if errors:
        for error in errors:
            print(f"- {error}", file=sys.stderr)
        return 1

    print(
        "Public surface inventory OK: "
        f"{len(module_to_group)} exposed modules mapped across {len(groups)} feature groups."
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
