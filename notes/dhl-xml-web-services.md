# DHL XML Web Services Outline

## Request Format

Here is the format for the request:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<req:KnownTrackingRequest xmlns:req="http://www.dhl.com" 
xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
xsi:schemaLocation="http://www.dhl.com
TrackingRequestKnown.xsd">
	<Request>
		<ServiceHeader>
            <SiteID>PUT_SITE_ID_HERE</SiteID>
            <Password>PUT_PASSWORD_HERE</Password>
		</ServiceHeader>
	</Request>
	<LanguageCode>en</LanguageCode>
	<AWBNumber>PUT_AWB_HERE</AWBNumber>
	<LevelOfDetails>ALL_CHECK_POINTS</LevelOfDetails>
	<PiecesEnabled>B</PiecesEnabled> 
</req:KnownTrackingRequest>
```

Let's dive into the various elements:

### `<Request>`

#### `<ServiceHeader>`

##### `<SiteID>`

Put your unique site ID here, which is provided by DHL.

##### `<Password>`

Put your password here, as provided by DHL.

### `<LanguageCode>`

This should be `en` for English.

### `<AWBNumber>`

Put your AWB Number here, which is used for tracking and provided by the factory when a shipment is sent.

### `<LevelOfDetails>`

There are two options for this element:

- `LAST_CHECK_POINT_ONLY`
- `ALL_CHECK_POINTS`

### `<PiecesEnabled>`

There are three options for this element:

- `S` - only shipment details
- `B` - both shipment and piece details
- `P` - only piece details

## Response Format

### `<Response>`

#### `<ServiceHeader>`

##### `<MessageTime>`

This element contains the time at which the message was sent by the requestor.

The format should be YYYY-MM-DD(T)hh-mm-ss-Time Zone, where -T is the separator between date and time.

##### `<SiteID>`

The site ID element is used to identify the requestor of the message.
