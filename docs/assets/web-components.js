import {html} from 'https://cdn.skypack.dev/lit';
import {component} from 'https://cdn.skypack.dev/haunted';

function Navigation({next, previous}) {
    return previous ? html`
        <div class="d-flex justify-content-between my-4">
            <a class="btn btn-outline-secondary" href="${previous}">Previous</a>
            ${next && html`<a class="btn btn-outline-secondary" href="${next}">Next</a>`}
        </div>` : html`
        <div class="text-end my-4">
            <a class="btn btn-outline-secondary" href="${next}">Next</a>
        </div>`;
}

customElements.define('tp-nav', component(Navigation, {useShadowDOM: false, observedAttributes: ['next', 'previous']}))