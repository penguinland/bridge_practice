async function getJson(url) {
    try {
        const response = await fetch(url, {method: "GET"});
        if (!response.ok) {
            console.log(error);
        }
        const data = await response.json();
        return data;
    } catch (error) {
        console.log(error);
    }
}

async function getTopics() {
    return await getJson("/topics");
}

async function getSituation() {
    const checkboxes = document.getElementsByName("topics");
    const indices = Array.prototype.slice.call(checkboxes)
        .filter(ch => ch.checked == true)
        .map(t => t.value)
        .join(",");
    return await getJson("/situation/" + indices);
}

getTopics().then(topics => {
    const topic_section = document.getElementById("topic_list")
    Object.entries(topics).forEach(([index, name]) => {
        const tag_name = "topic_" + index;
        const checkbox = document.createElement("input");
        checkbox.type  = "checkbox"
        checkbox.id    = tag_name;
        checkbox.name  = "topics";
        checkbox.value = index;
        if (index <= 2) {
            checkbox.checked = true; // Start off with a couple defaults
        }

        const label = document.createElement("label");
        label.for       = tag_name;
        label.innerHTML = name;

        newline = document.createElement("br")

        topic_section.appendChild(checkbox);
        topic_section.appendChild(label);
        topic_section.appendChild(newline);
    })

    displayProblem();
})

function makeBiddingRow(bids, type) {
    row = document.createElement("tr");
    bids.forEach(value => {
        item = document.createElement(type);
        item.innerHTML = value;
        item.width = "25%";
        row.appendChild(item);
    })
    return row
}

function displayBidding(bids) {
    table = document.createElement("table");
    table.style = "table-layout:fixed;";
    table.appendChild(makeBiddingRow(["West", "North", "East", "South"], "th"));
    bids.forEach(round => {
        table.appendChild(makeBiddingRow(round, "td"));
    })

    bidding = document.getElementById("bidding");
    if (bidding.firstChild) {
        bidding.removeChild(bidding.firstChild);
    }
    bidding.appendChild(table);
}

function setValue(id, val) {
    elem = document.getElementById(id);
    elem.innerHTML = val;
}

async function displayProblem () {
    getSituation().then(sitInst => {
        sit = document.getElementById("current_situation");
        sit.innerHTML = JSON.stringify(sitInst);
        displayBidding(sitInst.bidding);
        setValue("dealer", sitInst.deal.dealer);
        setValue("vulnerability", sitInst.deal.vulnerability);
    })
}
