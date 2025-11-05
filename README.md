# 🤝 Decentralized Freelance Work Escrow

A Clarity smart contract for secure freelance work agreements on the Stacks blockchain. This contract enables trustless escrow services between clients and freelancers with built-in dispute resolution.

## 🌟 Features

- ✅ **Secure Escrow**: Funds are locked in the contract until work completion
- 🎯 **Project Management**: Create, fund, and track freelance projects
- ⏰ **Deadline Enforcement**: Automatic protections for both parties
- 🛡️ **Dispute Resolution**: Built-in voting and admin resolution system
- 💰 **Emergency Withdrawals**: Freelancer protection after deadline + grace period
- 📊 **Project Tracking**: Complete project lifecycle management

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Stacks wallet for testing

### Installation

1. Clone this repository
2. Navigate to the project directory
3. Run Clarinet commands to deploy and test

```bash
clarinet check
clarinet test
clarinet deploy
```

## 📋 Contract Functions

### Public Functions

#### 🆕 `create-project`
Creates a new freelance project with specified parameters.
```clarity
(create-project freelancer amount deadline title description)
```

#### 💳 `fund-project`
Client funds the project, moving STX to escrow.
```clarity
(fund-project project-id)
```

#### 🏁 `start-work`
Freelancer accepts and starts working on the project.
```clarity
(start-work project-id)
```

#### 📤 `submit-work`
Freelancer submits completed work for review.
```clarity
(submit-work project-id)
```

#### ✅ `approve-work`
Client approves work and releases payment to freelancer.
```clarity
(approve-work project-id)
```

#### ⚖️ `dispute-work`
Either party can initiate a dispute for submitted work.
```clarity
(dispute-work project-id)
```

#### 🗳️ `vote-dispute`
Parties vote on dispute resolution.
```clarity
(vote-dispute project-id vote-for-freelancer)
```

#### 🔨 `resolve-dispute`
Contract owner resolves disputes (admin function).
```clarity
(resolve-dispute project-id favor-freelancer)
```

#### ❌ `cancel-project`
Client cancels project (conditions apply).
```clarity
(cancel-project project-id)
```

#### 🚨 `emergency-withdraw`
Freelancer withdraws funds after deadline + grace period.
```clarity
(emergency-withdraw project-id)
```

### Read-Only Functions

- `get-project`: Retrieve project details
- `get-project-funds`: Check escrowed funds
- `get-user-projects`: List user's projects
- `get-dispute-votes`: View dispute voting status
- `get-project-counter`: Total projects created
- `get-contract-balance`: Contract's STX balance

## 🔄 Project Lifecycle

1. **Created** → Client creates project
2. **
