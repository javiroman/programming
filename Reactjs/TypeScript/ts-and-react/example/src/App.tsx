import React from 'react';
import CounterOutput from './CounterOutput';

// README: https://www.reddit.com/r/reactjs/comments/5dxpzx/extending_component_vs_making_a_const/
//const App: React.FC = () => {
//  return (
//      <div style={{textAlign: 'center'}}>
//        <CounterOutput counter={1}/>
//        <button>Increment</button>
//        <button>Decrement</button>
//      </div>
//  );
//}

//interface IAppProps { }

interface IAppState {
    counterValue: number;
}

class App extends React.Component<{}, IAppState> {
    public state={counterValue: 0}

    public render() {
        return (
            <div style={{textAlign: 'center'}}>
                <CounterOutput counter={this.state.counterValue}/>
                <button onClick={this.incHandler}>Increment</button>
                <button onClick={this.decHandler}>Decrement</button>
            </div>
        );
    }

    private incHandler = () => {
        this.setState(prevState => {
            return {counterValue: prevState.counterValue + 1};
        });
    };

    private decHandler = () => {
        this.setState(prevState => {
            return {counterValue: prevState.counterValue - 1};
        });
    };
}

export default App;
