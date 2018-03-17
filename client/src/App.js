import React, { Component } from 'react';
import './App.css';

class Row extends Component {
  onChange = (e) => {
    this.props.onChange(this.props.index, {
      id:         this.props.id,
      date:       this.refs.date.value,
      type:       this.refs.type.value,
      from:       this.refs.from.value,
      fromAmount: this.refs.fromAmount.value,
      to:         this.refs.to.value,
      toAmount:   this.refs.toAmount.value,
      // costBasis:  this.refs.costBasis.value
    });
  }



  render() {
    const event = this.props;
    const { types, coins, fiat } = this.props.options;
    const { date, type, from, fromAmount, to, toAmount } = this.props;

    const fromTypes = type === 'PurchaseEvent' ? fiat : coins;
    const toTypes = type === 'PurchaseEvent' ? coins : fiat;

    return (
      <div className="row">
        <input type='text' onChange={this.onChange} defaultValue={date} placeholder='yyyy-mm-dd' ref='date'/>

        <select onChange={this.onChange} defaultValue={type} placeholder='type' ref='type'>
          <option key='' value=''>Select...</option>
          {types.map(x => <option key={x.value} value={x.value}>{x.name}</option>)}
        </select>

        <select onChange={this.onChange} defaultValue={from} disabled={!type} placeholder='from' ref='from'>
          <option key='' value=''>Select...</option>
          {fromTypes.map(x => <option key={x.value} value={x.value}>{x.name}</option>)}
        </select>

        <input className="numeric" type='text' onChange={this.onChange} defaultValue={fromAmount} disabled={!type} placeholder='amount' ref='fromAmount'/>

        <select onChange={this.onChange} defaultValue={to} disabled={!type} placeholder='to' ref='to'>
          <option key='' value=''>Select...</option>
          {toTypes.map(x => <option key={x.value} value={x.value}>{x.name}</option>)}
        </select>

        <input className="numeric" type='text' onChange={this.onChange} defaultValue={toAmount} disabled={!type} placeholder='amount' ref='toAmount'/>

        <button onClick={x => this.props.addRow(this.props.index)}>+</button>
        <button onClick={x => this.props.removeRow(this.props.index)}>-</button>
      </div>
    )
  }
}

// {
//   "tag": "PurchaseEvent",
//   "contents": {
//     "purchaseCrypto": {
//       "tag": "BTC"
//     },
//     "purchasePrice": "1000.0",
//     "purchaseTime": "2017-12-01T00:00:00Z",
//     "purchaseFiat": "USD",
//     "purchaseAmount": "1.0",
//     "purchaseFee": "1.0",
//     "purchaseExchange": "Coinbase"
//   }
// }
//
// {
//   "tag": "SaleEvent",
//   "contents": {
//     "saleCrypto": {
//       "tag": "BTC"
//     },
//     "salePrice": "1100.0",
//     "saleTime": "2017-12-01T01:00:00Z",
//     "saleFiat": "USD",
//     "saleAmount": "0.8",
//     "saleFee": "1.0",
//     "saleExchange": "Coinbase"
//   }
// }
//


class App extends Component {
  componentWillMount() {
    const initialEvents = [{
      tag: 'PurchaseEvent',
      contents: {
        purchaseCrypto: {
          tag: 'BTC'
        },
        purchasePrice: '1000.0',
        purchaseTime: '2017-12-01T00:00:00Z',
        purchaseFiat: 'USD',
        purchaseAmount: '1.0',
        purchaseFee: '1.0',
        purchaseExchange: 'Coinbase'
      }
    }, {
      tag: 'SaleEvent',
      contents: {
        saleCrypto: {
          tag: 'BTC'
        },
        salePrice: '1100.0',
        saleTime: '2017-12-01T01:00:00Z',
        saleFiat: 'USD',
        saleAmount: '0.8',
        saleFee: '1.0',
        saleExchange: 'Coinbase'
      }
    }];

    const initialRows = [{
      id: 1,
      date: '12-01-2017 12:00:00',
      type: 'PurchaseEvent',
      from: 'USD',
      fromAmount: 1000,
      to: 'BTC',
      toAmount: 1.5,
      costBasis: 1000
    }, {
      id: 2,
      date: '12-01-2017 12:00:00',
      type: 'SaleEvent',
      from: 'BTC',
      fromAmount: 1.0,
      to: 'USD',
      toAmount: 2000,
      costBasis: 2000
    }];

    const rows = [
      ...initialRows,
      this.newRow(3)
    ];

    this.setState({ rows });
  }

  test = () => {

  }

  newRow = (id) => {
    return {
      id,
      date: null,
      type: null,
      from: null,
      fromAmount: null,
      to: null,
      toAmount: null,
      costBasis: null
    }
  }

  addRow = (index) => {
    const rows   = this.state.rows;
    const before = rows.slice(0, index + 1);
    const after  = rows.slice(index + 1, rows.length);

    const updated = [...before, this.newRow(rows.length + 1), ...after];

    this.setState({rows: updated});
  }

  removeRow = (index) => {
    const rows = this.state.rows;
    const before = rows.slice(0, index);
    const after = rows.slice(index + 1, rows.length);

    const updated = [...before, ...after];

    this.setState({rows: updated});
  }

  sendRows = () => {
    console.log(this.state.rows);
  }

  onInputChange = (index, row) => {
    const rows = this.state.rows.map((r, i) => i === index ? row : r);

    this.setState({rows})
  }

  render() {
    const opt  = (name, value) => ({name, value});
    const opt2 = (name) => opt(name, name);

    const options = {
      types: [
        opt('Buy', 'PurchaseEvent'),
        opt('Sell', 'SaleEvent'),
        opt('Trade', 'TradeEvent'),
        opt('Transfer', 'TransferEvent')
      ],
      coins: [
        opt2('BTC'),
        opt2('ETH'),
        opt2('LTC')
      ],
      fiat: [
        opt2('USD')
      ]
    };

    const rows = this.state.rows;

    console.log(this);

    return (
      <div className='App'>
        <div>
          <span>Date</span>
          <span>Type</span>
          <span>From</span>
          <span>Amount</span>
          <span>To</span>
          <span>Amount</span>
        </div>
        {rows.map((x, i) =>
            <Row
              key={x.id}
              index={i}
              onChange={this.onInputChange}
              addRow={this.addRow}
              removeRow={this.removeRow}
              options={options}
              {...x}
            />
        )}
        <button onClick={this.sendRows}>Compute taxable events</button>
      </div>
    );
  }
}

export default App;
