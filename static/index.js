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
        const entry = document.createElement('span');
        entry.innerHTML = index + ": ";
        const italics = document.createElement('i');
        italics.innerHTML = name;
        newline = document.createElement('br')
        topic_section.appendChild(entry);
        topic_section.appendChild(italics);
        topic_section.appendChild(newline);
    })
})
