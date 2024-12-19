{-# LANGUAGE DuplicateRecordFields #-}

module DDD where

type UnvalidatedCustomerInfo = String
type UnvalidatedAddress = String
type DateTime = String

data UnvalidatedOrder = UnvalidatedOrder
  { orderId :: String
  , customerInfo :: UnvalidatedCustomerInfo
  , shippingAddress :: UnvalidatedAddress
  }

data InvalidOrder = InvalidOrder
  { orderId :: String
  , customerInfo :: UnvalidatedCustomerInfo
  , shippingAddress :: UnvalidatedAddress
  }


data PricedOrder = PricedOrder
  { orderId :: String
  , customerInfo :: UnvalidatedCustomerInfo
  , shippingAddress :: UnvalidatedAddress
  }

data Command d = Command
  { data' :: d
  , timestamp :: DateTime
  , userId :: String
  }

type PlaceOrder = Command UnvalidatedAddress

data Order = Invalid InvalidOrder
           | Unvalidated UnvalidatedOrder
           | Priced PricedOrder


type Item = String

data ShoppingCart = EmptyCart
                  | ActiveCart ActiveCartData
                  | PaidCart PaidCartData

data ActiveCartData = ActiveCartData
  { unpaidItems :: [Item]
  }

data PaidCartData = PaidCartData
  { paidItems :: [Item]
  , payment :: Float
  }

addItem :: ShoppingCart -> Item -> ShoppingCart
addItem cart item = case cart of
  EmptyCart           -> ActiveCart $ ActiveCartData [item]
  ActiveCart cartData -> ActiveCart $ ActiveCartData $ item : unpaidItems cartData
  PaidCart _           -> cart




type ProductCode = String
type ValidationError = String
type AddressValidationError = String

newtype CheckedAddress = CheckedAddress UnvalidatedAddress
data ValidatedOrder = ValidatedOrder

type CheckProductCodeExists = ProductCode -> Bool
type CheckAddressExists = UnvalidatedAddress -> Either AddressValidationError CheckedAddress

type ValidateOrder = CheckProductCodeExists                 -- dep
                  -> CheckAddressExists                     -- dep
                  -> UnvalidatedOrder                       -- input
                  -> Either ValidationError ValidatedOrder  -- output