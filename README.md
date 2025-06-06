# 🌱 Carbonex - Carbon Credit Marketplace

A decentralized marketplace for trading verifiable carbon offsets as NFTs on the Stacks blockchain. Trade, verify, and retire carbon credits with full transparency and immutable records.

## 🚀 Features

- 🏭 **Issuer Registration**: Carbon credit issuers can register and get verified
- 🎫 **NFT-based Credits**: Each carbon credit is a unique NFT with metadata
- 💰 **Marketplace Trading**: List, buy, and sell carbon credits
- ♻️ **Credit Retirement**: Permanently retire credits to offset carbon footprint
- 🔒 **Verification System**: Only verified issuers can mint credits
- 💸 **Platform Fees**: Configurable platform fees for sustainability

## 📋 Contract Functions

### Public Functions

#### For Issuers
- `register-issuer(name)` - Register as a carbon credit issuer
- `mint-carbon-credit(project-name, co2-amount, verification-standard, vintage-year)` - Mint new carbon credits

#### For Trading
- `list-credit-for-sale(credit-id, price)` - List a credit for sale
- `update-listing-price(credit-id, new-price)` - Update listing price
- `remove-listing(credit-id)` - Remove credit from marketplace
- `buy-carbon-credit(credit-id)` - Purchase a listed credit

#### For Users
- `retire-carbon-credit(credit-id)` - Permanently retire a credit

#### Admin Functions
- `verify-issuer(issuer)` - Verify a registered issuer (owner only)
- `set-platform-fee(new-fee)` - Set platform fee (owner only)

### Read-Only Functions

- `get-carbon-credit(credit-id)` - Get credit details
- `get-credit-listing(credit-id)` - Get listing information
- `get-issuer-info(issuer)` - Get issuer details
- `get-credit-owner(credit-id)` - Get current owner
- `get-user-retired-balance(user)` - Get user's retired credits count
- `get-platform-fee()` - Get current platform fee
- `get-next-credit-id()` - Get next available credit ID

## 🛠️ Usage Examples

### 1. Register as an Issuer
```clarity
(contract-call? .Carbonex register-issuer "Green Forest Initiative")
```

### 2. Mint Carbon Credits (after verification)
```clarity
(contract-call? .Carbonex mint-carbon-credit 
  "Amazon Reforestation Project" 
  u1000 
  "VCS" 
  u2023)
```

### 3. List Credit for Sale
```clarity
(contract-call? .Carbonex list-credit-for-sale u1 u50000000)
```

### 4. Buy Carbon Credit
```clarity
(contract-call? .Carbonex buy-carbon-credit u1)
```

### 5. Retire Carbon Credit
```clarity
(contract-call? .Carbonex retire-carbon-credit u1)
```

## 🔧 Setup & Deployment

### Prerequisites
- Clarinet CLI installed
- Stacks wallet for testing

### Local Development
```bash
clarinet check
```

```bash
clarinet test
```

```bash
clarinet console
```

### Deployment
```bash
clarinet deploy --testnet
```

## 💡 Key Concepts

- **Carbon Credits**: Represented as NFTs with metadata including CO2 amount, project details, and vintage year
- **Verification**: Only verified issuers can mint credits, ensuring quality and legitimacy
- **Retirement**: Credits can be permanently retired (burned) to claim carbon offset benefits
- **Marketplace**: Decentralized trading with transparent pricing and ownership transfer
- **Platform Fee**: Small fee (2.5% default) to support platform sustainability

## 🌍 Environmental Impact

Each carbon credit represents verified CO2 reduction or removal from the atmosphere. By trading and retiring these credits, users directly contribute to global climate action and carbon neutrality goals.

## 📊 Error Codes

- `u100` - Not authorized
- `u101` - Not found
- `u102` - Already exists
- `u103` - Invalid amount
- `u104` - Insufficient balance
- `u105` - Not owner
- `u106` - Invalid price
- `u107` - Credit not for sale

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch
3. Test your changes with Clarinet
4. Submit a pull request

## 📄 License

MIT License - Build the future of carbon markets! 🌱

