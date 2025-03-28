const { exec } = require("child_process");
const { promisify } = require("util");
const glob = require("glob");
const fs = require("fs");
const path = require("path");

const execPromise = promisify(exec);

async function runTests() {
  const results = [];
  const files = glob.sync("tests/**/*.stella");
  const maxTests = 1000; // Maximum number of tests to run
  let testCount = 0; // Counter for the number of tests run

  for (const file of files) {
    if (testCount >= maxTests) {
      console.log(`Reached the maximum of ${maxTests} tests. Stopping.`);
      break; // Stop after reaching the maximum number of tests
    }

    const isIllTyped = file.includes("ill-typed"); // Check if the test is ill-typed

    try {
      console.log(`Running test: ${file}`);
      await execPromise(`npm start ${file}`);
      if (isIllTyped) {
        results.push({
          file,
          status: "Failed",
          error: "Expected failure for ill-typed test, but it passed.",
        });
      } else {
        results.push({ file, status: "Passed" });
      }
    } catch (error) {
      console.error(`Test failed: ${file}`);
      const filteredError = extractErrorMessage(error.message);
      if (isIllTyped) {
        results.push({ file, status: "Passed", error: filteredError });
      } else {
        results.push({
          file,
          status: "Failed",
          error: "Expected success for well-typed test, but it failed.",
        });
      }
    }

    //testCount++; // Increment the test counter
  }

  // Write results to a file
  const resultsFilePath = path.join(__dirname, "test_results.json");
  fs.writeFileSync(resultsFilePath, JSON.stringify(results, null, 2));
  console.log(`Test results written to ${resultsFilePath}`);
}

function extractErrorMessage(fullError) {
  const match = fullError.match(/Error: [A-Z_]+/);
  return match ? match[0] : fullError; // Return the filtered error or the full error if no match
}

runTests();
