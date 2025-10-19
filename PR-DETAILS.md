# Carbon Credit Auction System

## Overview
Introduces a comprehensive time-based auction system for carbon credits, enabling competitive bidding and price discovery while maintaining security and transparency. This feature adds a new dimension to the marketplace by allowing credit owners to maximize value through auction dynamics while providing buyers with fair bidding opportunities.

## Technical Implementation

### Core Data Structures
- **auctions**: Primary auction data with credit ID, seller, pricing, timing, and status
- **auction-bids**: Individual bid tracking with refund status and timestamps  
- **auction-history**: Historical auction data for analytics and verification

### Key Functions Added
- `create-auction`: Start time-limited auctions with reserve pricing
- `place-bid`: Secure bidding with automatic previous bidder refunds
- `complete-auction`: Finalize auction with credit transfer and payment
- `cancel-auction`: Allow seller cancellation when no bids exist
- `emergency-refund`: Safety mechanism for failed auction recovery

### Advanced Features  
- **Automatic Refunds**: Previous highest bidders automatically refunded on new bids
- **Reserve Price Logic**: Configurable minimum price requirements
- **Time Management**: Block height-based auction duration with remaining time calculations
- **Value Estimation**: Dynamic pricing estimates based on current bid and time factors
- **Security Controls**: Prevention of self-bidding, expired auction interactions, and unauthorized actions

## Testing & Validation
- ✅ Contract passes clarinet check
- ✅ Clarity v3 compliant with proper error handling (8 new error constants: u200-u207)
- ✅ CI/CD pipeline configured with GitHub Actions
- ✅ Comprehensive input validation and authorization checks
- ✅ Independent feature with no cross-contract dependencies

## Security Features
- STX escrow system for bid amounts
- Automatic refund mechanism prevents fund loss  
- Emergency refund capability for edge cases
- Authorization checks prevent unauthorized auction operations
- Time-based controls prevent manipulation of expired auctions
