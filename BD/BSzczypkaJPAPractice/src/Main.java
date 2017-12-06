import org.hibernate.*;
import org.hibernate.query.Query;
import org.hibernate.cfg.Configuration;

import javax.persistence.metamodel.EntityType;

import java.util.Map;

public class Main {
    private static final SessionFactory ourSessionFactory;

    static {
        try {
            Configuration configuration = new Configuration();
            configuration.configure();

            ourSessionFactory = configuration.buildSessionFactory();
        } catch (Throwable ex) {
            throw new ExceptionInInitializerError(ex);
        }
    }

    public static Session getSession() throws HibernateException {
        return ourSessionFactory.openSession();
    }

    public static void main(final String[] args) throws Exception {
        final Session session = getSession();
        Transaction tx = session.beginTransaction();

//        Supplier supplier = new Supplier();
//        supplier.setCity("Krakow");
//        supplier.setCompanyName("AGH");
//        supplier.setStreet("Kawiory");
//
//
//        Product product = new Product();
//        product.setProductName("Kasza");
//        product.setUnitsOnStock(10);
//
//        session.save(product);
//        session.save(supplier);

        Product p = session.get(Product.class,1);
        Supplier s = session.get(Supplier.class, 1);
        p.setSupplier(s);


        tx.commit();

        session.close();
    }
}