import { test, expect } from "@playwright/test";

test.describe("Call Graph Explorer", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/");
  });

  test("shows toolbar with inputs", async ({ page }) => {
    await expect(
      page.getByPlaceholder("owner/repo")
    ).toBeVisible();
    await expect(
      page.getByPlaceholder("ref (main)")
    ).toHaveValue("main");
    await expect(
      page.getByPlaceholder("GitHub token")
    ).toBeVisible();
    await expect(
      page.getByRole("button", { name: "Load" })
    ).toBeVisible();
    await expect(
      page.getByRole("button", { name: "Fit" })
    ).toBeVisible();
  });

  test("shows placeholder when no graph loaded", async ({
    page,
  }) => {
    await expect(page.locator("#placeholder")).toHaveText(
      /call-graph\.dot/
    );
  });

  test("shows error on invalid repo format", async ({
    page,
  }) => {
    await page
      .getByPlaceholder("owner/repo")
      .fill("invalid");
    await page
      .getByRole("button", { name: "Load" })
      .click();
    await expect(page.locator("#error-bar")).toHaveText(
      /owner\/repo/
    );
  });

  test("repo input accepts owner/repo text", async ({
    page,
  }) => {
    const input = page.getByPlaceholder("owner/repo");
    await input.fill("lambdasistemi/chain-follower");
    await expect(input).toHaveValue(
      "lambdasistemi/chain-follower"
    );
  });

  test("ref input is editable", async ({ page }) => {
    const input = page.getByPlaceholder("ref (main)");
    await input.fill("docs/call-graph");
    await expect(input).toHaveValue("docs/call-graph");
  });

  test("sidebar shows instructions", async ({ page }) => {
    await expect(
      page.locator("#sidebar")
    ).toContainText("Click a node");
  });
});

test.describe("Graph loading", () => {
  test.skip(
    !process.env.GH_TOKEN,
    "GH_TOKEN required"
  );

  test.beforeEach(async ({ page }) => {
    await page.goto("/");
    await page
      .getByPlaceholder("owner/repo")
      .fill("lambdasistemi/chain-follower");
    await page
      .getByPlaceholder("ref (main)")
      .fill("docs/call-graph");
    await page
      .getByPlaceholder("GitHub token")
      .fill(process.env.GH_TOKEN!);
    await page
      .getByRole("button", { name: "Load" })
      .click();
    // Wait for graph to render
    await page.waitForTimeout(5000);
  });

  test("loads graph from GitHub", async ({ page }) => {
    // Placeholder should be gone
    await expect(
      page.locator("#placeholder")
    ).not.toBeVisible();
    // Focus and Fit buttons should not show Focus yet
    // (no node selected)
    await expect(
      page.getByRole("button", { name: "Fit" })
    ).toBeVisible();
  });

  test("clicking a node shows source code", async ({
    page,
  }) => {
    // Tap a node programmatically
    await page.evaluate(() => {
      const cy = (
        document.getElementById("cy") as any
      )._cyreg.cy;
      const node = cy
        .nodes()
        .filter(
          (n: any) =>
            n.data("label") === "composedFollowing"
        );
      if (node.length > 0) node.emit("tap");
    });
    await page.waitForTimeout(3000);

    // Sidebar should show the function name
    await expect(
      page.locator("#sidebar h3")
    ).toHaveText("composedFollowing");
    // Kind badge
    await expect(
      page.locator(".kind-badge")
    ).toHaveText("function");
    // Module path
    await expect(
      page.locator(".module-path")
    ).toContainText("tutorial/Composed.hs");
    // Source code
    await expect(
      page.locator("#source-code")
    ).toContainText("module Composed");
  });

  test("Focus shows neighborhood subgraph", async ({
    page,
  }) => {
    // Select a node
    await page.evaluate(() => {
      const cy = (
        document.getElementById("cy") as any
      )._cyreg.cy;
      const node = cy
        .nodes()
        .filter(
          (n: any) =>
            n.data("label") === "composedFollowing"
        );
      if (node.length > 0) node.emit("tap");
    });
    await page.waitForTimeout(1000);

    // Click Focus
    await page
      .getByRole("button", { name: "Focus" })
      .click();
    await page.waitForTimeout(2000);

    // Show All button should appear
    await expect(
      page.getByRole("button", { name: "Show All" })
    ).toBeVisible();
    // Depth controls should appear
    await expect(page.locator("#depth-ctrl")).toContainText(
      "Depth: 1"
    );
  });
});
