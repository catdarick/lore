export type CustomUiComponent = {
  render: (width: number) => string[];
  handleInput: (data: string) => void;
  invalidate: () => void;
};

export type CustomUiTheme = {
  fg?: (role: string, text: string) => string;
  bold?: (text: string) => string;
};

export type CustomUiKeybindings = {
  matches: (data: string, keybinding: string) => boolean;
};

export type LoreMenuUi = {
  select?: (title: string, options: string[]) => Promise<string | undefined>;
  custom?: <T>(factory: (...args: unknown[]) => unknown) => Promise<T | undefined>;
};

export async function selectLoreMenu(ui: LoreMenuUi, title: string, options: string[]): Promise<string | undefined> {
  if (ui.custom) {
    return ui.custom<string>((_tui, theme, keybindings, done) =>
      framedCustomUi(
        new LoreMenuComponent(title, options, theme as CustomUiTheme, keybindings as CustomUiKeybindings, done),
        theme as CustomUiTheme,
        "↑↓ navigate  enter select  escape/ctrl+c cancel",
      ),
    );
  }
  if (ui.select) return ui.select(title, options);
  throw new Error("Lore menu requires Pi custom or select UI support");
}

export function framedCustomUi(
  component: CustomUiComponent,
  theme: CustomUiTheme | undefined,
  footer: string,
): CustomUiComponent {
  return {
    render(width) {
      const safeWidth = Number.isFinite(width) && width > 0 ? Math.floor(width) : 80;
      const separator = styleUiText(theme, "border", "─".repeat(safeWidth));
      const bodyWidth = Math.max(1, safeWidth - 2);
      const body = component.render(bodyWidth).map((line) => line.length === 0 ? "" : ` ${line}`);
      const footerLine = styleUiText(theme, "dim", ` ${footer.slice(0, bodyWidth)}`);
      return [separator, "", ...body, "", footerLine, "", separator];
    },
    handleInput(data) {
      component.handleInput(data);
    },
    invalidate() {
      component.invalidate();
    },
  };
}

export function styleUiTitle(theme: CustomUiTheme | undefined, text: string): string {
  const bold = theme?.bold?.(text) ?? text;
  return styleUiText(theme, "accent", bold);
}

export function styleUiSelection(theme: CustomUiTheme | undefined, text: string, selected: boolean): string {
  return selected ? styleUiText(theme, "accent", text) : text;
}

export function styleUiText(theme: CustomUiTheme | undefined, role: string, text: string): string {
  return theme?.fg?.(role, text) ?? text;
}

class LoreMenuComponent implements CustomUiComponent {
  private readonly title: string;
  private readonly options: string[];
  private readonly theme: CustomUiTheme;
  private readonly keybindings: CustomUiKeybindings;
  private readonly done: (value: string | undefined) => void;
  private cursor = 0;

  constructor(
    title: string,
    options: string[],
    theme: CustomUiTheme,
    keybindings: CustomUiKeybindings,
    done: (value: string | undefined) => void,
  ) {
    this.title = title;
    this.options = options;
    this.theme = theme;
    this.keybindings = keybindings;
    this.done = done;
  }

  render(width: number): string[] {
    const safeWidth = Number.isFinite(width) && width > 0 ? Math.floor(width) : 80;
    const titleLines = this.title.split(/\r?\n/);
    const firstHeading = titleLines.findIndex((line) => line.trim().length > 0);
    const renderedTitle = titleLines.map((line, index) => {
      const clipped = line.slice(0, safeWidth);
      return index === firstHeading ? styleUiTitle(this.theme, clipped) : clipped;
    });
    const options = this.options.map((option, index) => {
      const selected = index === this.cursor;
      const line = `${selected ? "→" : " "} ${option}`.slice(0, safeWidth);
      return styleUiSelection(this.theme, line, selected);
    });
    return [...renderedTitle, "", ...options];
  }

  handleInput(data: string): void {
    if (this.keybindings.matches(data, "tui.select.down") || data === "j") {
      this.cursor = Math.min(this.cursor + 1, this.options.length - 1);
      return;
    }
    if (this.keybindings.matches(data, "tui.select.up") || data === "k") {
      this.cursor = Math.max(this.cursor - 1, 0);
      return;
    }
    if (this.keybindings.matches(data, "tui.select.confirm")) {
      this.done(this.options[this.cursor]);
      return;
    }
    if (this.keybindings.matches(data, "tui.select.cancel")) this.done(undefined);
  }

  invalidate(): void {
    // stateless render cache
  }
}
