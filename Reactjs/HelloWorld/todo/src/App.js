import React, {Component} from 'react';

class App extends Component {
    constructor(props) {
        super(props);
        this.state = {
            userName: "Adam"
        }
    }

    changeStateData = () => {
        this.setState({
            userName: this.state.userName === "Adam" ? "Bob" : "Adam"
        })
    }

    render() {
        return (
            <div>
                <h4 className="bg-primary text-white text-center p-2">
                    { this.state.userName }'s Do List
                </h4>
                <button className="btn btn-primary m-2"
                        onClick={ this.changeStateData}>
                    Change
                </button>
            </div>
        );
    }
}

export default App;
