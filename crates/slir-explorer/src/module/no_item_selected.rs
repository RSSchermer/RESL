use leptos::prelude::*;

#[component]
pub fn NoItemSelected() -> impl IntoView {
    view! {
        <div class="info-page-container">
            <h1>"No Item Selected"</h1>

            <p>"Use the menu on the left to select a module item to inspect."</p>
        </div>
    }
}
