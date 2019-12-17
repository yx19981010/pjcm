package com.samsph.pjcm.config.utils;

import com.google.common.collect.Lists;
import com.samsph.pjcm.config.PageData;
import com.samsph.pjcm.config.PageDataT;
import org.dozer.DozerBeanMapper;
import org.springframework.data.domain.Page;

import java.util.Collection;
import java.util.List;

/**
 * @author hujiahao
 */
public class DozerUtil {
    private static DozerBeanMapper dozerBeanMapper = new DozerBeanMapper();

    public static <T> List<T> mapList(Collection sourceList, Class<T> destinationClass) {
        List destinationList = Lists.newArrayList();
        for (Object sourceObject : sourceList) {
            Object destinationObject = dozerBeanMapper.map(sourceObject, destinationClass);
            destinationList.add(destinationObject);
        }
        return destinationList;
    }

    public static <T> T map(Object sourceObject, Class<T> destinationClass) {
        return dozerBeanMapper.map(sourceObject, destinationClass);
    }

    public static <T> PageData mapPage(Page page, Class<T> destinationClass) {
        return new PageData(
                page.getTotalPages(),
                (int) page.getTotalElements(),
                page.getNumber() + 1,
                page.getNumberOfElements(),
                mapList(page.getContent(), destinationClass));
    }

    public static <T> PageDataT mapPageT(Page page, Class<T> destinationClass) {
        return new PageDataT<T>(
                page.getTotalPages(),
                (int) page.getTotalElements(),
                page.getNumber() + 1,
                page.getNumberOfElements(),
                mapList(page.getContent(), destinationClass));
    }
}
