pragma solidity ^0.8.25;

library Verification {
    function Assume(bool b) internal pure {}

    function Assume_Complex(string memory req) internal pure {}

    function Pause() internal pure {}
}

interface IERC20 {
    /**

     * @dev Emitted when `value` tokens are moved from one account (`from`) to

     * another (`to`).

     *

     * Note that `value` may be zero.

     */

    event Transfer(address indexed from, address indexed to, uint256 value);

    /**

     * @dev Emitted when the allowance of a `spender` for an `owner` is set by

     * a call to {approve}. `value` is the new allowance.

     */

    event Approval(
        address indexed owner,
        address indexed spender,
        uint256 value
    );

    /**

     * @dev Returns the amount of tokens in existence.

     */

    function totalSupply() external view returns (uint256);

    /**

     * @dev Returns the amount of tokens owned by `account`.

     */

    function balanceOf(address account) external view returns (uint256);

    /**

     * @dev Moves `amount` tokens from the caller's account to `to`.

     *

     * Returns a boolean value indicating whether the operation succeeded.

     *

     * Emits a {Transfer} event.

     */

    function transfer(address to, uint256 amount) external returns (bool);

    /**

     * @dev Returns the remaining number of tokens that `spender` will be

     * allowed to spend on behalf of `owner` through {transferFrom}. This is

     * zero by default.

     *

     * This value changes when {approve} or {transferFrom} are called.

     */

    function allowance(
        address owner,
        address spender
    ) external view returns (uint256);

    /**

     * @dev Sets `amount` as the allowance of `spender` over the caller's tokens.

     *

     * Returns a boolean value indicating whether the operation succeeded.

     *

     * IMPORTANT: Beware that changing an allowance with this method brings the risk

     * that someone may use both the old and the new allowance by unfortunate

     * transaction ordering. One possible solution to mitigate this race

     * condition is to first reduce the spender's allowance to 0 and set the

     * desired value afterwards:

     * https://github.com/ethereum/EIPs/issues/20#issuecomment-263524729

     *

     * Emits an {Approval} event.

     */

    function approve(address spender, uint256 amount) external returns (bool);

    /**

     * @dev Moves `amount` tokens from `from` to `to` using the

     * allowance mechanism. `amount` is then deducted from the caller's

     * allowance.

     *

     * Returns a boolean value indicating whether the operation succeeded.

     *

     * Emits a {Transfer} event.

     */

    function transferFrom(
        address from,
        address to,
        uint256 amount
    ) external returns (bool);
}

abstract contract Context {
    function _msgSender() internal view virtual returns (address) {
        return msg.sender;
    }

    function _msgData() internal view virtual returns (bytes calldata) {
        return msg.data;
    }
}

abstract contract Ownable is Context {
    address private _owner;

    event OwnershipTransferred(
        address indexed previousOwner,
        address indexed newOwner
    );

    /**

     * @dev Initializes the contract setting the deployer as the initial owner.

     */

    constructor() {
        _transferOwnership(_msgSender());
    }

    /**

     * @dev Throws if called by any account other than the owner.

     */

    modifier onlyOwner() {
        _checkOwner();

        _;
    }

    /**

     * @dev Returns the address of the current owner.

     */

    function owner() public view virtual returns (address) {
        return _owner;
    }

    /**

     * @dev Throws if the sender is not the owner.

     */

    function _checkOwner() internal view virtual {
        require(owner() == _msgSender(), "Ownable: caller is not the owner");
    }

    /**

     * @dev Leaves the contract without owner. It will not be possible to call

     * `onlyOwner` functions. Can only be called by the current owner.

     *

     * NOTE: Renouncing ownership will leave the contract without an owner,

     * thereby disabling any functionality that is only available to the owner.

     */

    function renounceOwnership() public virtual onlyOwner {
        _transferOwnership(address(0));
    }

    /**

     * @dev Transfers ownership of the contract to a new account (`newOwner`).

     * Can only be called by the current owner.

     */

    function transferOwnership(address newOwner) public virtual onlyOwner {
        require(
            newOwner != address(0),
            "Ownable: new owner is the zero address"
        );

        _transferOwnership(newOwner);
    }

    /**

     * @dev Transfers ownership of the contract to a new account (`newOwner`).

     * Internal function without access restriction.

     */

    function _transferOwnership(address newOwner) internal virtual {
        address oldOwner = _owner;

        _owner = newOwner;

        emit OwnershipTransferred(oldOwner, newOwner);
    }
}

contract Crowdfunding is Ownable {
    enum CampaignStatus {
        Active,
        Inactive,
        Successful,
        Failed
    }

    int constant Active = 0;
    int constant Inactive = 1;
    int constant Successful = 2;
    int constant Failed = 3;

    struct Campaign {
        address creator;
        string title;
        string description;
        uint256 goalAmount;
        uint256 raisedAmount;
        uint256 deadline;
        int status;
        mapping(address => uint256) contributions;
        address[] backers;
        uint256 totalContributors;
    }

    Campaign[] public campaigns;
    mapping(address => uint256)[] campaigns_contributions;
    address[] campaigns_creator;
    string[] campaigns_title;
    string[] campaigns_description;
    uint256[] campaigns_goalAmount;
    uint256[] campaigns_raisedAmount;
    uint256[] campaigns_deadline;
    int[] campaigns_status;
    address[] campaigns_backers;
    uint256[] campaigns_totalContributors;
    mapping(address => uint256[]) public creatorCampaigns;
    mapping(uint256 => mapping(address => bool)) public hasClaimedReward;

    IERC20 public tokenContract;
    uint256 public withdrawableBalance;
    uint256 public minimumContribution;
    uint256 public maxCampaignDuration;
    uint256 public maxCampaignsPerUser;
    event CampaignCreated(
        uint256 indexed campaignId,
        address indexed creator,
        string title,
        uint256 goalAmount,
        uint256 deadline
    );
    event ContributionMade(
        uint256 indexed campaignId,
        address indexed contributor,
        uint256 amount
    );
    event CampaignFinished(uint256 indexed campaignId, int status);
    event RewardClaimed(
        uint256 indexed campaignId,
        address indexed backer,
        uint256 amount
    );

    modifier onlyContributor(uint256 _campaignId) {
        require(
            campaigns_contributions[_campaignId][msg.sender] > 0,
            "You are not a contributor to this campaign"
        );
        _;
    }
    modifier onlyInState(uint256 _campaignId, int _state) {
        require(
            campaigns_status[_campaignId] == _state,
            "Invalid campaign state"
        );
        _;
    }

    constructor(
        address _tokenContract,
        uint256 _minimumContribution,
        uint256 _maxCampaignDuration,
        uint256 _maxCampaignsPerUser
    ) {
        tokenContract = IERC20(_tokenContract);
        minimumContribution = _minimumContribution;
        maxCampaignDuration = _maxCampaignDuration;
        maxCampaignsPerUser = _maxCampaignsPerUser;
    }

    function createCampaign(
        string memory _title,
        string memory _description,
        uint256 _goalAmount,
        uint256 _duration
    ) public {
        require(
            _duration <= maxCampaignDuration,
            "Campaign duration exceeds maximum limit"
        );
        require(
            campaigns.length < maxCampaignsPerUser || maxCampaignsPerUser == 0,
            "Maximum number of campaigns reached"
        );
        uint256 deadline = block.timestamp + _duration;
        uint256 campaignId = campaigns.length;
        creatorCampaigns[msg.sender].push(campaignId);
        emit CampaignCreated(
            campaignId,
            msg.sender,
            _title,
            _goalAmount,
            deadline
        );
    }

    function contribute(uint256 _campaignId) public payable {
        require(
            msg.value >= minimumContribution,
            "Contribution amount must be greater than or equal to minimum"
        );
        // Campaign storage campaign = campaigns[_campaignId];
        require(
            block.timestamp < campaigns_deadline[_campaignId],
            "Campaign deadline has passed"
        );
        campaigns_raisedAmount[_campaignId] += msg.value;
        campaigns_contributions[_campaignId][msg.sender] += msg.value;
        if (campaigns_contributions[_campaignId][msg.sender] == msg.value) {
            campaigns_totalContributors[_campaignId]++;
            // campaigns_backers[_campaignId].push(msg.sender);
        }
        emit ContributionMade(_campaignId, msg.sender, msg.value);
        if (
            campaigns_raisedAmount[_campaignId] >=
            campaigns_goalAmount[_campaignId]
        ) {
            campaigns_status[_campaignId] = Successful;
            emit CampaignFinished(_campaignId, Successful);
        }
    }

    function withdrawFunds(
        uint256 _campaignId
    ) public onlyOwner onlyInState(_campaignId, Successful) {
        Campaign storage campaign = campaigns[_campaignId];
        require(
            campaigns_raisedAmount[_campaignId] > 0,
            "No funds to withdraw"
        );
        require(tokenContract != IERC20(address(0)), "Token contract not set");
        tokenContract.transfer(
            campaigns_creator[_campaignId],
            campaigns_raisedAmount[_campaignId]
        );
        campaigns_status[_campaignId] = Inactive;
        emit CampaignFinished(_campaignId, Inactive);
    }

    function claimReward(
        uint256 _campaignId
    ) public onlyContributor(_campaignId) onlyInState(_campaignId, Successful) {
        Campaign storage campaign = campaigns[_campaignId];
        require(
            !hasClaimedReward[_campaignId][msg.sender],
            "Reward already claimed"
        );
        uint256 contribution = campaigns_contributions[_campaignId][msg.sender];
        uint256 totalAmount = campaigns_raisedAmount[_campaignId];
        uint256 reward = (contribution * campaigns_goalAmount[_campaignId]) /
            totalAmount;
        hasClaimedReward[_campaignId][msg.sender] = true;
        if (tokenContract != IERC20(address(0))) {
            tokenContract.transfer(msg.sender, reward);
        } else {
            payable(msg.sender).transfer(reward);
        }
        emit RewardClaimed(_campaignId, msg.sender, reward);
    }

    function refund(
        uint256 _campaignId
    ) public onlyContributor(_campaignId) onlyInState(_campaignId, Failed) {
        Campaign storage campaign = campaigns[_campaignId];
        require(
            block.timestamp >= campaigns_deadline[_campaignId],
            "Refunds not available yet"
        );
        require(
            campaigns_contributions[_campaignId][msg.sender] > 0,
            "You have not contributed to this campaign"
        );
        uint256 amount = campaigns_contributions[_campaignId][msg.sender];
        campaigns_raisedAmount[_campaignId] -= amount;
        campaigns_contributions[_campaignId][msg.sender] = 0;
        payable(msg.sender).transfer(amount);
    }

    function getCampaignCount() public view returns (uint256) {
        return campaigns.length;
    }

    function getCampaignDetails(
        uint256 _campaignId
    )
        public
        view
        returns (
            address,
            string memory,
            string memory,
            uint256,
            uint256,
            uint256,
            int
        )
    {
        return (
            campaigns_creator[_campaignId],
            campaigns_title[_campaignId],
            campaigns_description[_campaignId],
            campaigns_goalAmount[_campaignId],
            campaigns_raisedAmount[_campaignId],
            campaigns_deadline[_campaignId],
            campaigns_status[_campaignId]
        );
    }

    function getBackersCount(
        uint256 _campaignId
    ) public view returns (uint256) {
        return campaigns[_campaignId].backers.length;
    }

    function getBackerAddress(
        uint256 _campaignId,
        uint256 _index
    ) public view returns (address) {
        return campaigns[_campaignId].backers[_index];
    }
}
