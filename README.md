# 🌳 Tokenized Carbon Credits Marketplace

A transparent and accessible marketplace for trading carbon credits on the Stacks blockchain.

## 🎯 Features

- Create verified carbon credit projects
- Mint tokenized carbon credits
- Transfer credits between participants
- Retire carbon credits
- Track project and credit details

## 🚀 Contract Functions

### Administrative Functions
- `create-project`: Create new carbon offset projects
- `mint-credit`: Mint new carbon credits for verified projects

### User Functions
- `transfer-credit`: Transfer carbon credits between participants
- `retire-credit`: Permanently retire carbon credits
- `get-credit`: View credit details
- `get-project`: View project details
- `get-credit-owner`: Check current credit owner

## 💡 Usage Example

1. Admin creates a new carbon project:
```clarity
(contract-call? .tokenized-carbon-credits-marketplace create-project "Amazon Reforestation" "Brazil" u1000)
```

2. Admin mints credits for verified projects:
```clarity
(contract-call? .tokenized-carbon-credits-marketplace mint-credit u1 u100 u500 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7)
```

3. Users can transfer credits:
```clarity
(contract-call? .tokenized-carbon-credits-marketplace transfer-credit u1 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7)
```

4. Retire credits:
```clarity
(contract-call? .tokenized-carbon-credits-marketplace retire-credit u1)
```

## 🔒 Security

- Only contract owner can create projects and mint credits
- Credits can only be transferred by their current owner
- Retired credits cannot be transferred
```

