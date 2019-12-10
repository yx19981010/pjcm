package com.samsph.pjcm.config.utils;

import com.google.common.collect.Lists;
import org.dozer.DozerBeanMapper;

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

    public static <T> T map(Object sourceObject,Class<T> destinationClass){
        return dozerBeanMapper.map(sourceObject, destinationClass);
    }
}
