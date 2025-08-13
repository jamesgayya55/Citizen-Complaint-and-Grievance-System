# Citizen Complaint and Grievance System (CCG)

A decentralized application built on Stacks blockchain that allows citizens to submit, track, and resolve complaints with full transparency.

## Overview

The Citizen Complaint and Grievance System (CCG) is designed to provide a transparent and immutable platform for citizens to submit complaints or grievances. The system ensures that all complaints are properly tracked, can be voted on by other citizens to indicate importance, and can be resolved by authorized administrators.

## Features

- Submit complaints with title, description, and category
- Track complaint status (pending, in-progress, resolved, rejected)
- Vote for complaints to indicate community support
- Comment on complaints for additional context
- Resolve complaints with resolution notes
- View complaint history and status

## Smart Contract Functions

### User Functions

- `submit-complaint`: Submit a new complaint with title, description, and category
- `vote-for-complaint`: Vote for a complaint to increase its priority
- `add-comment`: Add a comment to an existing complaint
- `get-complaint`: Get details of a specific complaint
- `get-user-complaints`: Get all complaints submitted by a specific user
- `get-total-complaints`: Get the total number of complaints in the system
- `get-comment`: Get a specific comment on a complaint
- `get-comment-count`: Get the number of comments on a complaint
- `has-voted`: Check if a user has voted for a specific complaint

### Admin Functions

- `resolve-complaint`: Resolve a complaint with resolution notes
- `update-complaint-status`: Update the status of a complaint
- `transfer-ownership`: Transfer contract ownership to a new administrator

## Usage Examples

### Submit a Complaint

```clarity
(contract-call? .ccg submit-complaint "Broken Street Light" "The street light at Main St and 5th Ave has been out for two weeks" "Infrastructure")
```

### Vote for a Complaint

```clarity
(contract-call? .ccg vote-for-complaint u1)
```

### Add a Comment

```clarity
(contract-call? .ccg add-comment u1 "I've noticed this issue as well. It's creating a safety hazard at night.")
```

### Resolve a Complaint (Admin Only)

```clarity
(contract-call? .ccg resolve-complaint u1 "Maintenance team has replaced the bulb and fixed the wiring issue. The light is now operational.")
```

## Error Codes

- `u100`: Not authorized
- `u101`: Invalid status
- `u102`: Complaint not found
- `u103`: Complaint already resolved
- `u104`: Empty title
- `u105`: Empty description
- `u106`: Too many complaints
- `u107`: Already voted
- `u108`: Empty comment

## Deployment

This contract can be deployed using Clarinet or directly on the Stacks blockchain.

```bash
clarinet console
```

Then within the Clarinet console:

```clarity
(contract-call? .ccg submit-complaint "Test Complaint" "This is a test" "Test")
```