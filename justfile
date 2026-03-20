build:
    spago build

bundle:
    spago bundle

dev:
    spago build --watch

format:
    purs-tidy format-in-place src/**/*.purs

lint:
    purs-tidy check src/**/*.purs

ci: lint build bundle

test: bundle
    npx playwright test

test-auth: bundle
    GH_TOKEN=$(gh auth token) npx playwright test

serve: bundle
    npx serve dist -p 10001

clean:
    rm -rf output/ dist/index.js
