async function getTopics() {
    try {
        const response = await fetch('/topics', {method:'GET'});
        if (!response.ok) {
            console.log(error);
        }
        const data = await response.json();
        return data;
    } catch (error) {
        console.log(error);
    }
}

getTopics().then(topics => {
    const topic_section = document.getElementById('topic_list')
    Object.entries(topics).forEach(([index, name]) => {
        const tag_name = "topic_" + index;
        const checkbox = document.createElement('input');
        checkbox.type  = "checkbox"
        checkbox.id    = tag_name;
        checkbox.name  = tag_name;
        checkbox.value = name;

        const label = document.createElement('label');
        label.for       = tag_name;
        label.innerHTML = name;

        newline = document.createElement('br')

        topic_section.appendChild(checkbox);
        topic_section.appendChild(label);
        topic_section.appendChild(newline);
    })
})
