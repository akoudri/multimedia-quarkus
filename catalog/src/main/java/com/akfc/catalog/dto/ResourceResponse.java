package com.akfc.catalog.dto;

import com.akfc.catalog.data.Resource;
import com.akfc.catalog.data.ResourceStatus;
import com.akfc.catalog.data.ResourceType;

import java.time.LocalDateTime;
import java.util.List;

/**
 * DTO for resource response.
 * Contains all resource information for API responses.
 */
public class ResourceResponse {

    public Long id;
    public String title;
    public ResourceType type;
    public Integer year;
    public String creator;
    public List<String> keywords;
    public String illustrationUrl;
    public ResourceStatus status;
    public Boolean archived;
    public LocalDateTime createdAt;
    public LocalDateTime updatedAt;

    /**
     * Create response DTO from entity.
     */
    public static ResourceResponse from(Resource resource) {
        ResourceResponse response = new ResourceResponse();
        response.id = resource.id;
        response.title = resource.title;
        response.type = resource.type;
        response.year = resource.year;
        response.creator = resource.creator;
        response.keywords = resource.keywords;
        response.illustrationUrl = resource.illustrationUrl;
        response.status = resource.status;
        response.archived = resource.archived;
        response.createdAt = resource.createdAt;
        response.updatedAt = resource.updatedAt;
        return response;
    }
}
