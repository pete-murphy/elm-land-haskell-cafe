import "@github/relative-time-element"

// This returns the flags passed into your Elm application
export const flags = async ({ env } : ElmLand.FlagsArgs) => {
  return {}
}

// This function is called after your Elm app starts
export const onReady = ({ app, env } : ElmLand.OnReadyArgs) => {
  console.log('Elm is ready', app)
}


// Type definitions for Elm Land
namespace ElmLand {
  export type FlagsArgs = {
    env: Record<string, string>
  }
  export type OnReadyArgs = {
    env: Record<string, string>
    app: { ports?: Record<string, Port> }
  }
  export type Port = {
    send?: (data: unknown) => void
    subscribe?: (callback: (data: unknown) => unknown) => void
  }
}

class LocaleTime extends HTMLElement {
  static get observedAttributes() {
    return ['datetime']
  }
  options: Intl.DateTimeFormatOptions
  constructor() {
    super()
  }
  connectedCallback() {
    this.render()
  }
  attributeChangedCallback(name: string, oldValue: string, newValue: string) {
    this.render()
  }

  render() {
    const date = new Date(this.getAttribute('datetime')!)
    this.textContent = date.toLocaleString(undefined, this.options)
  }
}

customElements.define('locale-time', LocaleTime)