#!/bin/bash
# test_compatibility.sh
# Compatibility Test Suite für Linux/Mac
# Prüft Case-Sensitivity, Pfade und Encoding

set -e

echo "=== TDM-Tool v4 Compatibility Test ==="
echo "Testing on: $(uname -s)"
echo "Date: $(date)"
echo ""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Test counter
TESTS_PASSED=0
TESTS_FAILED=0

# Function to run a test
run_test() {
    local test_name="$1"
    local test_command="$2"
    
    echo -n "Testing $test_name... "
    
    if eval "$test_command" > /dev/null 2>&1; then
        echo -e "${GREEN}OK${NC}"
        ((TESTS_PASSED++))
        return 0
    else
        echo -e "${RED}FAILED${NC}"
        ((TESTS_FAILED++))
        return 1
    fi
}

# Test 1: File Extensions
echo "=== Testing File Extensions ==="
run_test "no .r files exist" '[ $(find R/ -name "*.r" -type f 2>/dev/null | wc -l) -eq 0 ]'
run_test "all R files use .R extension" '[ $(find R/ -name "*.R" -type f 2>/dev/null | wc -l) -gt 0 ]'

# Test 2: Source References
echo ""
echo "=== Testing Source References ==="
run_test "no source() with .r extension" '! grep -r "source.*\.r[\"\']" --include="*.R" R/ 2>/dev/null'
run_test "no source() with absolute R/ paths in R files" '! grep -r "source.*[\"\'']R/" --include="*.R" R/ 2>/dev/null'

# Test 3: Path Separators
echo ""
echo "=== Testing Path Separators ==="
run_test "no backslashes in paths" '! grep -r "\\\\" --include="*.R" R/ | grep -v "\\\\n\|\\\\t\|\\\\r" 2>/dev/null'
run_test "no Windows drive letters" '! grep -r "[A-Z]:" --include="*.R" R/ 2>/dev/null'

# Test 4: UTF-8 Encoding
echo ""
echo "=== Testing UTF-8 Encoding ==="
run_test "R files are UTF-8 or ASCII" 'for f in $(find R/ -name "*.R"); do file "$f" | grep -q "UTF-8\|ASCII" || exit 1; done'
run_test "markdown files are UTF-8 or ASCII" 'for f in $(find . -name "*.md"); do file "$f" | grep -q "UTF-8\|ASCII" || exit 1; done'

# Test 5: Critical Files
echo ""
echo "=== Testing Critical Files ==="
run_test "app.R exists" '[ -f app.R ]'
run_test "R/load_all.R exists" '[ -f R/load_all.R ]'
run_test "R/utils.R exists" '[ -f R/utils.R ]'
run_test ".editorconfig exists" '[ -f .editorconfig ]'

# Test 6: R Syntax Check
echo ""
echo "=== Testing R Syntax ==="
if command -v Rscript > /dev/null 2>&1; then
    run_test "app.R syntax valid" 'Rscript -e "tryCatch(parse(\"app.R\"), error=function(e) quit(status=1))"'
    run_test "load_all.R syntax valid" 'Rscript -e "tryCatch(parse(\"R/load_all.R\"), error=function(e) quit(status=1))"'
    
    # Test 7: Load Test
    echo ""
    echo "=== Testing Module Loading ==="
    run_test "utils.R can be sourced" 'Rscript -e "source(\"R/utils.R\")"'
    run_test "load_all.R can be sourced" 'Rscript -e "source(\"R/load_all.R\")"'
else
    echo -e "${YELLOW}WARNING: Rscript not found - skipping R syntax tests${NC}"
fi

# Test 8: File Name Conflicts
echo ""
echo "=== Testing File Name Conflicts ==="
run_test "no duplicate files (case-insensitive)" '
    duplicates=$(find R/ -type f -name "*.R" -o -name "*.r" 2>/dev/null | 
                 xargs -I {} basename {} | 
                 tr "[:upper:]" "[:lower:]" | 
                 sort | uniq -d | wc -l)
    [ "$duplicates" -eq 0 ]
'

# Test 9: Module References
echo ""
echo "=== Testing Module References ==="
run_test "no broken module references" '
    ! grep -r "source\|library\|require" --include="*.R" R/ 2>/dev/null | 
    grep -E "(utils/config|utils/helpers|plots/plot_concentrations|reports/report_generator)" 
'

# Test 10: API References
echo ""
echo "=== Testing API References ==="
if [ -d "api" ]; then
    run_test "api files use relative paths" '! grep -r "source.*[\"\'']R/" --include="*.R" api/ 2>/dev/null'
else
    echo -e "${YELLOW}No api/ directory found - skipping API tests${NC}"
fi

# Summary
echo ""
echo "======================================="
echo "=== Test Summary ==="
echo "======================================="
echo -e "Tests passed: ${GREEN}$TESTS_PASSED${NC}"
echo -e "Tests failed: ${RED}$TESTS_FAILED${NC}"

if [ $TESTS_FAILED -eq 0 ]; then
    echo -e "${GREEN}✓ All compatibility tests passed!${NC}"
    echo "The system is ready for Linux/Mac deployment."
    exit 0
else
    echo -e "${RED}✗ Some tests failed.${NC}"
    echo "Please fix the issues before deploying to Linux/Mac."
    exit 1
fi