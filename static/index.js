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

    bidding = clear("bidding");
    bidding.appendChild(table);
}

function setValue(id, val) {
    elem = document.getElementById(id);
    elem.innerHTML = val;
}

var current_problem = null;

function clear(id) {
    elem = document.getElementById(id);
    while (elem.firstChild) {
        elem.removeChild(elem.firstChild);
    }
    return elem;
}

async function displayProblem () {
    getSituation().then(problem => {
        displayBidding(problem.bidding);
        setValue("dealer", problem.deal.dealer);
        setValue("vulnerability", problem.deal.vulnerability);
        displayHand(problem.deal.south_hand, "south_hand");
        clear("west_hand");
        clear("east_hand");
        clear("north_hand");
        current_problem = problem;

        show_ans = document.createElement("button");
        show_ans.innerHTML = "Confirm Bid";
        show_ans.onclick = show_answer;
        expl = clear("explanation");
        expl.appendChild(show_ans);
    })
}

function displayHand(hand, id) {
    elem = document.getElementById(id);
    elem.innerHTML = "&spades;&nbsp;&nbsp;" + hand.spades + "<br/>" +
                     "<span style='color: red'>&hearts;</span>&nbsp;&nbsp;" +
                         hand.hearts + "<br/>" +
                     "<span style='color: red'>&diams;</span>&nbsp;&nbsp;" +
                         hand.diamonds + "<br/>" +
                     "&clubs;&nbsp;&nbsp;" + hand.clubs;
    elem.style = "word-spacing:-0.1em;"
}

function show_answer() {
    displayHand(current_problem.deal.west_hand, "west_hand");
    displayHand(current_problem.deal.east_hand, "east_hand");
    displayHand(current_problem.deal.north_hand, "north_hand");

    expl = clear("explanation");
    ans = document.createElement("p");
    ans.innerHTML = "Answer: " + current_problem.answer;
    expl.appendChild(ans);
    expl.appendChild(document.createElement("br"));
    sol = document.createElement("p");
    sol.innerHTML = current_problem.explanation;
    expl.appendChild(sol);
    expl.appendChild(document.createElement("br"));
    dbg = document.createElement("p")
    dbg.innerHTML = "debug string: " + current_problem.debug_string;
    expl.appendChild(dbg);
}
