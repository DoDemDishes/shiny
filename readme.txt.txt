REQUIREMENTS FOR THE PREPARATION OF FILES

1) All the information should match the column name under which it is given.
2) Depending on the type of action, the names of the relevant columns should be as belows. If any of the names is missing, it should be added as an empty column:
	- NEW LEADS: company_name, amazon_merchant_id, amazon_feedback, ebay_username, webstore_url, webstore_traffic, category, platform, country, first_name, last_name, phone, email, language, lead_source, lead_status, marketing_campaign_2, webstore_platform, predicted_ebay_gmv, predicted_amazon_gmv, predicted_webstore_gmv, ebay_item_location, ebay_posts_to
	- CONTACT UPDATE: amazon_merchant_id, ebay_username, country, first_name, last_name, email, phone, agent_signature, contact_details_update, status
	- GMV UPDATE: ebay_username, amazon_merchant_id, predicted_ebay_gmv, amazon_feedback
3) All the names of the columns should be lowercase. Words should be separated by an underscore(symbol "_"). For example, "First Name" should be changed into "first_name"
4) For CONTACT UPDATE, the column "status" should be filled with relevant symbols.